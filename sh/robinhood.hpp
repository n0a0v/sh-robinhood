/*	BSD 3-Clause License

	Copyright (c) 2022, Paul Varga
	All rights reserved.

	Redistribution and use in source and binary forms, with or without
	modification, are permitted provided that the following conditions are met:

	1. Redistributions of source code must retain the above copyright notice, this
	   list of conditions and the following disclaimer.

	2. Redistributions in binary form must reproduce the above copyright notice,
	   this list of conditions and the following disclaimer in the documentation
	   and/or other materials provided with the distribution.

	3. Neither the name of the copyright holder nor the names of its
	   contributors may be used to endorse or promote products derived from
	   this software without specific prior written permission.

	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
	AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
	IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
	DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
	FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
	DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
	SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
	CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
	OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
	OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef INC_SH__ROBINHOOD_HPP
#define INC_SH__ROBINHOOD_HPP

/**	@file
 *	This file declares facilities for use with Robinhood-style open addressing
 *	hash tables (map, set). Robinhood hashing aims to ameliorate the downsides
 *	of open addressing by reducing the probe length variance.
 *
 *	This implementation keeps the Robinhood "info" separate from the stored
 *	"data" (key & value) in order to potentially reduce memory usage (alignment
 *	padding) and improve performance (reduced cache misses).
 *
 *	Additionally, "info" may be configured to cache a truncated hash (one byte,
 *	by default) for particular key types that opt-in. This can both reduce the
 *	need to access "data" where misses are frequent and reduce the number of
 *	comparisons for find operations. Specialize the cache_hash template to
 *	control this on a per-key type basis; this is already configured here for
 *	std::basic_string, for which it's nearly always beneficial.
 *
 *	Unlike std::unordered_map & std::unordered_set, the size type dictating the
 *	container's max_size can be configured. It defaults to std::uint32_t,
 *	allowing for 4,294,967,294 entries (0xFFFF'FFFF minus 1 for the special
 *	value zero used to indicate "empty" in the Robinhood distance). This can be
 *	altered via template parameter to support greater (e.g., std::size_t) or
 *	lesser capacity.
 *
 *	Maps and sets built on this table have a weakened exception guarantee from
 *	std::unordered_map & std::unordered_set. Those generally offer a "strong
 *	exception guarantee" that if a function throws an exception the state will
 *	be rolled back to one identical to before the function call. This
 *	implementation instead promises in similar places an "intermediate
 *	guarantee" slightly stronger than the "basic exception guarantee" -- that
 *	if a function throws an exception, the program is in a valid state and that
 *	resources are not leaked -- by additionally promising that the number of
 *	and associations of key/pair relationships remain intact but without
 *	promising that the order of elements or iterators will remain intact. This
 *	is a consolation for better performance; rewind operations are used on
 *	failure to avoid extra swaps and copies.
 *
 *	This implementation has more strict requirements for noexcept than many
 *	containers. Key & value types must provided noexcept move construction and
 *	also be noexcept swappable. The hasher type must not throw when operator()
 *	is called but this is not enforced at compile time (like the standard
 *	containers)! Both the hasher and key_equal types must be noexcept move
 *	assignable and swappable for move assignment and swap, respectively.
 *
 *	Other performance considerations include:
 *	a. The empty distance is zero, allowing less-than comparison to include empty.
 *	b. Iteration of elements is avoided where not necessary, either because of
 *	   zero constructed values or because values have trivial destructors.
 *	c. For trivial "data" keys/values, bulk memory operations (memset, memcpy)
 *	   are used when preferable.
 *	d. Storing an extra element as a "tail" for "info", allowing loops to
 *	   overrun the last element without having to condition on end-index for each
 *	   iteration. See "info_tail_v" below.
 *	e. Using the narrowest "info" type as possible. When there is no cached
 *	   truncated hash, this is configured to be one byte. This opens up the
 *	   possibility of badly-performing hash algorithms creating excessive
 *	   collision and distance exceeding the storage maximum of 255. As a
 *	   contingency, a wide "info" type is automatically (but pessimistically)
 *	   available as a fallback.
 *
 *	Numerous runtime asserts are enabled if NDEBUG is not defined due to
 *	SH_ROBINHOOD_ASSERT being defined to use the standard assert. It can
 *	be hollowed out if this is a performance issue during debugging.
 *
 *	Iterator invalidation:
 *		Never:
 *			* at, find, read operations, write to existing entry's value.
 *		Never with stateless or equivalent or swap-propagated allocators:
 *			* swap (iterators become valid for the swapped container).
 *		On rehash:
 *			* reserve.
 *		On insert:
 *			* emplace, emplace_hint, insert, insert_or_assign, operator[],
 *			  try_emplace, try_emplace_hint.
 *		Always:
 *			* clear, erase, copy/move operator=, rehash.
 *
 *		Excepting end(), which is only invalidated when the operation causes
 *		a rehash/resize.
 *
 *	Hence, there are more operations that can invalidate iterators than most
 *	similar containers. Users may define SH_ROBINHOOD_DEBUG_ITERATOR to
 *	enable runtime checking of iterators (enabled by default if NDEBUG is not
 *	defined).
 *
 *	Benchmarks:
 *	* Map types denoted as key => value.
 *	* Lower numbers are better.
 *	* Load factors left at implementation default.
 *	* Compared against libstdc++6-10's map types as they provide a low bar.
 *	* Compared against Google's absl::flat_hash_map as it provides a high bar.
 *	* absl::flat_hash_map may reduce the buffer size when clear() is called.
 *	  The documentation mentions to use erase(begin(), end()) as an alternative.
 *	  Each appeared to have its place; small values (keys, in particular)
 *	  performed better with clear, while retaining the buffer space performed
 *	  better for large values.
 *
 *	* Insertion of 1,000,000 random entries into reserved space, cleared every
 *	  other fill (fill, clear, fill, fill, clear...) with 32 fills. This
 *	  ensures over-writes on the secondary fills. Smaller key types will
 *	  necessarily have a greater number of identical keys.
 *
 *	  Google's absl::flat_hash_map is generally faster to insert with
 *	  decreasing advantage as value (in particular, key) size increases. As
 *	  integer key size reaches 8 bytes, openmap's insert performance is close
 *	  and better with very large key sizes. Performance for string keys is
 *	  within a few percent with absl::flat_hash_map leading.
 *
 *		* std::uint16_t => std::uint16_t:
 *			openmap:            	911,460,817
 *			absl::flat_hash_map:	449,904,184 with clear()
 *			absl::flat_hash_map:	720,522,949 with erase(begin(), end())
 *			std::unordered_map: 	1,443,548,172
 *			std::map:           	6,066,540,333
 *		* std::uint32_t => std::uint32_t:
 *			openmap:            	1,087,004,860
 *			absl::flat_hash_map:	935,513,208 with clear()
 *			absl::flat_hash_map:	1,294,113,511 with erase(begin(), end())
 *			std::unordered_map: 	3,104,744,703
 *			std::map:           	18,766,364,260
 *		* std::string => std::uint32_t:
 *			openmap:            	4,115,395,030
 *			absl::flat_hash_map:	4,020,320,115 with clear()
 *			absl::flat_hash_map:	4,073,279,697 with erase(begin(), end())
 *			std::unordered_map: 	8,742,941,854
 *			std::map:           	23,754,806,961
 *		* std::uint16_t => std::uint64_t:
 *			openmap:            	780,398,301
 *			absl::flat_hash_map:	504,504,717 with clear()
 *			absl::flat_hash_map:	923,453,582 with erase(begin(), end())
 *			std::unordered_map: 	1,433,706,396
 *			std::map:           	6,103,783,428
 *		* std::uint64_t => std::uint16_t:
 *			openmap:            	1,222,187,299
 *			absl::flat_hash_map:	1,208,781,310 with clear()
 *			absl::flat_hash_map:	1,394,933,430 with erase(begin(), end())
 *			std::unordered_map: 	3,078,923,629
 *			std::map:           	18,577,207,490
 *		* std::uint64_t => std::uint64_t:
 *			openmap:            	1,228,177,278
 *			absl::flat_hash_map:	1,204,096,303 with clear()
 *			absl::flat_hash_map:	1,369,679,460 with erase(begin(), end())
 *			std::unordered_map: 	3,072,681,816
 *			std::map:           	18,469,323,159
 *		* std::uint64_t[12] => std::uint32_t:
 *			openmap:            	1,579,295,604
 *			absl::flat_hash_map:	2,558,071,072 with clear()
 *			absl::flat_hash_map:	1,699,781,336 with erase(begin(), end())
 *
 *	* Find of 1,000,000 random entries with a fixed percentage of failures
 *	  repeated 32 times. The failures are generated by omitting values during
 *	  table generation.
 *
 *	  When there are fewer than 25% find misses, openmap is fastest. Find
 *	  performance for absl::flat_hash_map improves relatively (75%) and then
 *	  absolutely (50%) as either the table is more empty and/or the frequency
 *	  of missed finds increases. Lookup time of std::string is very close, and
 *	  holds even with more find misses. This appears to indicate that the
 *	  info-cached hash value would benefit other data types when frequent find
 *	  misses are to be expected (see cache_hash below).
 *
 *		* std::uint32_t => std::uint32_t, 100% success:
 *			openmap:            	666,852,152
 *			absl::flat_hash_map:	836,379,890
 *			std::unordered_map: 	2,370,151,725
 *			std::map:           	11,505,618,290
 *		* std::uint32_t => std::uint32_t, 75% success:
 *			openmap:            	677,759,798
 *			absl::flat_hash_map:	865,454,007
 *			std::unordered_map: 	2,308,407,631
 *			std::map:           	10,836,974,695
 *		* std::uint32_t => std::uint32_t, 50% success:
 *			openmap:            	792,654,198
 *			absl::flat_hash_map:	674,027,393
 *			std::unordered_map: 	2,141,944,832
 *			std::map:           	9,434,106,168
 *		* std::uint64_t[12] => std::uint32_t, 100% success:
 *			openmap:            	1,460,918,969
 *			absl::flat_hash_map:	1,701,753,390
 *		* std::uint64_t[12] => std::uint32_t, 50% success:
 *			openmap:            	1,734,404,109
 *			absl::flat_hash_map:	1,442,033,237
 *		* std::string => std::uint32_t, 100% success:
 *			openmap:            	3,830,818,165
 *			absl::flat_hash_map:	4,185,509,584
 *		* std::string => std::uint32_t, 50% success:
 *			openmap:            	3,219,433,791
 *			absl::flat_hash_map:	3,249,688,875
 *
 *	* Erase of 1,000,000 random entries by key, repeated 32 times.
 *
 *	  Here, openmap appears to be generally the fastest.
 *
 *		* std::uint32_t => std::uint32_t:
 *			openmap:            	338,766,729
 *			absl::flat_hash_map:	401,623,149
 *			std::unordered_map: 	492,169,292
 *			std::map:           	672,978,181
 *		* std::uint64_t[12] => std::uint32_t:
 *			openmap:            	376,817,682
 *			absl::flat_hash_map:	441,826,852
 *		* std::string => std::uint32_t:
 *			openmap:            	1,678,544,139
 *			absl::flat_hash_map:	1,735,625,543
 *
 *	* Rehash of reserved and filled 1,000,000 entries by copying a given table
 *	  and then inserting 1,000,000 additional (different) entries. Repeated 32
 *	  times.
 *
 *	  Google's absl::flat_hash_map is faster to rehash. I believe this is due
 *	  to its generally quicker insert time.
 *
 *		* std::uint16_t => std::uint16_t:
 *			openmap:            	397,369,494
 *			absl::flat_hash_map:	440,555,878
 *		* std::uint32_t => std::uint32_t:
 *			openmap:            	1,751,234,961
 *			absl::flat_hash_map:	1,432,966,931
 *		* std::string => std::uint32_t:
 *			openmap:            	7,783,913,328
 *			absl::flat_hash_map:	5,728,761,689
 *
 *	* Copy of table of 1,000,000 entries into uninitialized destination
 *	  followed by destruction. Repeated 32 times.
 *
 *	  Here, openmap is dramatically quicker with trivial key & value data types
 *	  and somewhat faster otherwise.
 *
 *		* std::uint32_t => std::uint32_t:
 *			openmap:            	18,886,668
 *			absl::flat_hash_map:	129,167,085
 *			std::unordered_map: 	1,580,876,994
 *			std::map:           	1,676,165,710
 *		* std::uint64_t[12] => std::uint32_t:
 *			openmap:            	1,320,562,036
 *			absl::flat_hash_map:	1,386,165,724
 *		* std::string => std::uint32_t:
 *			openmap:            	733,653,166
 *			absl::flat_hash_map:	812,101,328
 *
 *	* Clear a copy of a table initialized with 1,000,000 entries. Repeated 32
 *	  times. The copy time is included.
 *
 *	  Here, openmap is dramatically quicker with trivial key & value data types
 *	  and a bit faster with non-trivial ones. As stated, absl::flat_hash_map
 *	  may reduce the buffer size on clear, increasing its test times.
 *	  Interestingly, doing a full erase seems to not be a performance gain in
 *	  these tests.
 *
 *		* std::uint32_t => std::uint32_t:
 *			openmap:            	17,819,944
 *			absl::flat_hash_map:	128,546,386 with clear()
 *			absl::flat_hash_map:	275,682,559 with erase(begin(), end())
 *			std::unordered_map: 	1,396,449,286
 *			std::map:           	1,693,583,547
 *		* std::uint64_t[12] => std::uint32_t:
 *			openmap:            	378,867,714
 *			absl::flat_hash_map:	1,381,572,313 with clear()
 *			absl::flat_hash_map:	1,516,424,229 with erase(begin(), end())
 *		* std::string => std::uint32_t:
 *			openmap:            	773,350,253
 *			absl::flat_hash_map:	816,377,594 with clear()
 *			absl::flat_hash_map:	879,750,507 with erase(begin(), end())
 *
 *	* Interate from beginning to end a table initialized with 1,000,000
 *	  entries. Repeated 32 times.
 *
 *	  Here, absl::flat_hash_map is the clear winner.
 *
 *		* std::uint32_t => std::uint32_t:
 *			openmap:            	127,852,497
 *			absl::flat_hash_map:	71,213,936
 *			std::unordered_map: 	455,033,879
 *			std::map:           	1,436,505,525
 *		* std::uint64_t[12] => std::uint32_t:
 *			openmap:            	144,346,327
 *			absl::flat_hash_map:	129,946,020
 *		* std::string => std::uint32_t:
 *			openmap:            	115,727,850
 *			absl::flat_hash_map:	83,332,435
 */

#include <algorithm>
#include <cassert>
#include <climits>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <functional>
#include <iterator>
#include <memory>
#include <string>
#include <type_traits>
#include <utility>

/**	Transparently wraps assert to allow asserts to be turned off for the sh Robinhood-style hashtables in one location, if too costly.
 */
#define SH_ROBINHOOD_ASSERT assert

#ifndef NDEBUG
	/**	If defined, sh Robinhood-style hashtable iterators will capture extra information at construction time that will be checked via SH_ROBINHOOD_ASSERT for validity. Operations (see list above) that invalidate iterators will fail the assertation.
	 */
	#define SH_ROBINHOOD_DEBUG_ITERATOR
#endif // !NDEBUG

// Macros for branch prediction suggestion:
#if defined(__has_builtin)
	#if __has_builtin(__builtin_expect)
		#define SH_ROBINHOOD_EXPECT(EXPRESSION, CONSTANT) (__builtin_expect((EXPRESSION), (CONSTANT)))
	#endif
#elif defined(__GNUC__) && !defined(__llvm__) && __GNUC__ >= 3
	// Older versions of GCC support __builtin_expect, but don't provide __has_builtin.
	#define SH_ROBINHOOD_EXPECT(EXPRESSION, CONSTANT) (__builtin_expect((EXPRESSION), (CONSTANT)))
#endif
#ifdef SH_ROBINHOOD_EXPECT
	#define SH_ROBINHOOD_LIKELY(EXPRESSION) SH_ROBINHOOD_EXPECT(!!(EXPRESSION), 1)
	#define SH_ROBINHOOD_UNLIKELY(EXPRESSION) SH_ROBINHOOD_EXPECT(!!(EXPRESSION), 0)
#else
	#define SH_ROBINHOOD_EXPECT(EXPRESSION, CONSTANT) (EXPRESSION)
	#define SH_ROBINHOOD_LIKELY(EXPRESSION) (EXPRESSION)
	#define SH_ROBINHOOD_UNLIKELY(EXPRESSION) (EXPRESSION)
#endif

// Macros for compiler warnings:
#if defined(__GNUC__) && (!defined(__llvm__) || __clang_major__ >= 14)
	#define SH_ROBINHOOD_WARN_APPEND(A, B) A ## B
	#define SH_ROBINHOOD_WARN(WHAT) \
		struct SH_ROBINHOOD_WARN_APPEND(SH_WARN_, __LINE__) \
		{ \
			SH_ROBINHOOD_WARN_APPEND(SH_WARN_, __LINE__)() __attribute__((warning(WHAT))) \
			{} \
		} SH_ROBINHOOD_WARN_APPEND(sh_warn_, __COUNTER__)
#else
	#define SH_ROBINHOOD_WARN(WHAT) /* warning: WHAT */
#endif

/**	A Robinhood-style hashtable implementation.
 */
namespace sh::robinhood
{
	/**	The default size_type for implementation hash tables.
	 *	@details Determines the maximum table size. Use std::size_t for maximum table size (at the potential cost of memory usage).
	 */
	using default_size_type = std::uint32_t;

	/**	Specializable template for robinhood hashtable keys to indicate if it's beneficial to cache the hash value.
	 *	@details Should be specialized to true for types that are expensive to hash and perhaps for tables where
	 *	find performance is a high priority and misses are expected to be very frequent.
	 *	@tparam Key A type of key used in a hashtable.
	 */
	template <typename Key>
	struct cache_hash final : std::false_type { };

	/**	Helper to briefly access value of cache_hash::value.
	 *	@tparam Key A type of key used in a hashtable.
	 */
	template <typename Key>
	constexpr bool cache_hash_v = cache_hash<Key>::value;

	/**	Specialized template to indicate std::basic_string (et al) should have their hash values cached.
	 *	@param This has proven to be nearly always a performance benefit for these types.
	 */
	template <typename CharT, typename Traits, typename Allocator>
	struct cache_hash<std::basic_string<CharT, Traits, Allocator>> final : std::true_type { };

	/**	Used to determine if a given type is marked is_transparent.
	 *	@details Used on the hasher and key_equal types to determine if they should accept arbitrary Key types.
	 *	@tparam T A hasher or key_equal type.
	 */
	template <typename T, typename = void>
	constexpr bool is_transparent_v = false;

	/**	Specialized template to determine if a given type is marked is_transparent by containing a type of that name.
	 *	@tparam T A hasher or key_equal type.
	 */
	template <typename T>
	constexpr bool is_transparent_v<T, std::void_t<typename T::is_transparent>> = true;

	/**	The number of empty info elements that are appended to the Robinhood info elements that correspond to actual keys & values.
	 *	@tparam SizeType The size_type of the hashtable buckets.
	 */
	template <typename SizeType>
	constexpr SizeType info_tail_v = 1;

	/**	The magic (sentinel) distance value that represents an empty Robinhood info.
	 *	@note Must be byte-settable (e.g., 0-255) as to be used by memset. Must also be the minimum value so that other distances compare greater-than (so, zero).
	 *	@tparam DistanceType The distance type of the Robinhood hashtable information elements.
	 */
	template <typename DistanceType>
	constexpr DistanceType empty_distance_v = 0;

	/**	Provides a fixed signed unsigned integer type from a given number of bytes.
	 *	@tparam Size The desired value of sizeof(uint_bytes<Size>::type).
	 */
	template <std::size_t Size> struct uint_bytes {};
	template <> struct uint_bytes<1> { using type = std::uint8_t; };
	template <> struct uint_bytes<2> { using type = std::uint16_t; };
	template <> struct uint_bytes<4> { using type = std::uint32_t; };
	template <> struct uint_bytes<8> { using type = std::uint64_t; };
	template <std::size_t Size> using uint_bytes_t = typename uint_bytes<Size>::type;

	/**	Finds the greatest power of two that is less than the input.
	 *	@param value The input value.
	 *	@returns The greatest integer value where 2^log2ui(v) <= v.
	 *	@tparam T An unsigned integer type.
	 */
	template <typename T>
	constexpr T log2ui(T value) noexcept
	{
		static_assert(std::is_unsigned_v<T>, "log2ui expects only unsigned integer values");
		T result = 0;
		while (value >>= 1)
		{
			++result;
		}
		return result;
	}
	/**	Rounds an unsigned integer value to the next higher power of two. An input of zero will return zero.
	 *	@param value The input value.
	 *	@returns The next higher power of two from value. Or, if value is zero, zero.
	 *	@tparam T An unsigned integer type.
	 */
	template <typename T>
	constexpr T ceilui_power_of_two_or_zero(T value) noexcept
	{
		static_assert(std::is_unsigned_v<T>, "ceilui_power_of_two_or_zero expects only unsigned integer values.");
		--value;
		for (T bits = 1; bits < sizeof(value)*CHAR_BIT; bits <<= 1)
		{
			value |= value >> bits;
		}
		++value;
		return value;
	}
	/**	Rounds an unsigned integer value to the next higher power of two.
	 *	@param value The input value.
	 *	@returns The next higher power of two from value. Or, if value is zero, one.
	 *	@tparam T An unsigned integer type.
	 */
	template <typename T>
	constexpr T ceilui_power_of_two(T value) noexcept
	{
		static_assert(std::is_unsigned_v<T>, "ceilui_power_of_two expects only unsigned integer values.");
		value = ceilui_power_of_two_or_zero(value);
		value += (value == 0);
		return value;
	}
	/**	Rounds a numeric value to the next highest integer.
	 *	@param value The input value.
	 *	@returns The next highest integer.
	 *	@tparam T The output type.
	 *	@tparam V The input type.
	 */
	template <typename T, typename V>
	constexpr T ceilui(V&& value) noexcept
	{
		static_assert(std::is_unsigned_v<T>, "ceilui expects to return only unsigned integer values.");
		return T(std::ceil(value));
	}

#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
	/**	Debug information to be captured in an iterator.
	 *	@tparam SizeType The size_type of the hashtable buckets.
	 */
	template <typename SizeType>
	struct iterator_debug final
	{
		using size_type = SizeType;

		/**	The debug generation at the time of iterator construction.
		 *	@details Each iterator-invalidating modification of the table buckets will cause this value to fail to match the new value.
		 */
		size_type m_generation = 0;

		/**	The bucket capacity at the time of iterator construction.
		 *	@details Any time the bucket capacity is altered can signal that an iterator is invalid.
		 */
		size_type m_capacity = 0;
	};
#endif // SH_ROBINHOOD_DEBUG_ITERATOR

	/**	Simple Robinhood hashtable information element containing a distance value.
	 *	@note A distance value of zero indicates empty (see empty_distance_v above).
	 *	@tparam DistanceType The distance type of the Robinhood hashtable information elements.
	 */
	template <typename DistanceType>
	class info final
	{
	public:
		static_assert(std::is_unsigned_v<DistanceType>, "Negative and non-integral DistanceType not supported.");

		using distance_type = DistanceType;

		// To maintain is_trivial status, a full suite of default constructors & assignment operators.
		info() = default;
		info(const info&) noexcept = default;
		info(info&&) noexcept = default;
		info& operator=(const info&) noexcept = default;
		info& operator=(info&&) noexcept = default;

		/**	Constructor for a given distance.
		 *	@param distance The distance value with which to initialize.
		 */
		constexpr explicit info(const distance_type distance) noexcept
			: m_distance(distance)
		{ }
		/**	Constructor for a given distance & ignored cached hash value.
		 *	@param distance The distance value with which to initialize.
		 *	@param cached_hash A cached hash value to ignore.
		 */
		constexpr info(const distance_type distance, const std::size_t cached_hash) noexcept
			: m_distance(distance)
		{ }

		/**	Copy constructor from a Robinhood info of a different DistanceType.
		 *	@note Does not check for narrowing conversion of the distance value.
		 */
		template <typename OtherDistanceType>
		constexpr info(const info<OtherDistanceType>& other) noexcept
			: m_distance(other.get_distance())
		{ }
		/**	Copy assignment operator from a Robinhood info of a different DistanceType.
		 *	@note Does not check for narrowing conversion of the distance value.
		 */
		template <typename OtherDistanceType>
		constexpr info& operator=(const info<OtherDistanceType>& other) noexcept
		{
			m_distance = other.get_distance();
			return *this;
		}

		/**	Swap operation with a given Robinhood info.
		 *	@param other The info with which to swap values.
		 */
		void swap(info& other) noexcept
		{
			using std::swap;
			swap(m_distance, other.m_distance);
		}
		/**	Swap two Robinhood info.
		 *	@param lhs An info to swap values with rhs.
		 *	@param rhs An info to swap values with lhs.
		 */
		friend void swap(info& lhs, info& rhs) noexcept
		{
			lhs.swap(rhs);
		}
		/**	Reset this Robinhood info to empty.
		 */
		constexpr void clear() noexcept
		{
			m_distance = empty_distance_v<distance_type>;
		}
		/**	Increment this Robinhood info's distance by 1.
		 */
		constexpr void push_forward() noexcept
		{
			++m_distance;
		}
		/**	Return true if this Robinhood info indicates an empty position.
		 */
		constexpr bool empty() const noexcept
		{
			return m_distance == empty_distance_v<distance_type>;
		}
		/**	Return the maximum distance value that may be stored in this Robinhood info.
		 *	@return the maximum distance value that may be stored in this Robinhood info.
		 */
		constexpr static distance_type max_distance() noexcept
		{
			return std::numeric_limits<distance_type>::max();
		}
		/**	Return this info's current distance. Will be zero if empty.
		 *	@return this info's current distance. Will be zero if empty.
		 */
		constexpr distance_type get_distance() const noexcept
		{
			return m_distance;
		}
		/**	Return a copy of this Robinhood info with a distance reduced by 1.
		 *	@return A copy of this Robinhood info with a distance reduced by 1.
		 */
		constexpr info move_back() const noexcept
		{
			SH_ROBINHOOD_ASSERT(m_distance > 1);
			return info(m_distance - 1);
		}
		/**	Return true if this Robinhood info's cached hash matches the given info's
		 *	@param other Another Robinhood info.
		 *	@return Always returns true.
		 */
		constexpr static bool cached_hash_equal(const info& /*other*/) noexcept
		{
			return true;
		}

	private:
		/**	The Robinhood distance (from expected place) as well as empty condition.
		 */
		distance_type m_distance;
	};

	/**	Simple Robinhood hashtable information element containing a distance value & a cached (possibly truncated) hash value.
	 *	@details Used when cache_hash_v for a key type is true.
	 *	@note A distance value of zero indicates empty (see empty_distance_v above).
	 *	@tparam DistanceType The distance type of the Robinhood hashtable information elements.
	 *	@tparam CachedHashType The type in which to store a cached (and potentially truncated) hash result.
	 */
	template <typename DistanceType, typename CachedHashType>
	class cached_hash_info final
	{
	public:
		static_assert(std::is_unsigned_v<DistanceType>, "Negative and non-integral DistanceType not supported.");
		static_assert(std::is_integral_v<CachedHashType>, "Non-integral CachedHashType not supported.");

		using distance_type = DistanceType;
		using cached_hash_type = CachedHashType;

		// To maintain is_trivial status, a full suite of default constructors & assignment operators.
		cached_hash_info() = default;
		cached_hash_info(const cached_hash_info&) noexcept = default;
		cached_hash_info(cached_hash_info&&) noexcept = default;
		cached_hash_info& operator=(const cached_hash_info&) noexcept = default;
		cached_hash_info& operator=(cached_hash_info&&) noexcept = default;

		/**	Constructor for a given distance & cached hash value.
		 *	@param distance The distance value with which to initialize.
		 *	@param cached_hash A cached hash value with which to initialize.
		 */
		constexpr cached_hash_info(const distance_type distance, const std::size_t cached_hash) noexcept
			: m_distance(distance)
			, m_cached_hash(cached_hash)
		{ }

		/**	Copy constructor from a Robinhood cached hash info of a different DistanceType.
		 *	@note Does not check for narrowing conversion of the distance value.
		 */
		template <typename OtherDistanceType>
		constexpr cached_hash_info(const cached_hash_info<OtherDistanceType, CachedHashType>& other) noexcept
			: m_distance(other.get_distance())
			, m_cached_hash(other.get_cached_hash())
		{ }
		/**	Copy assignment operator from a Robinhood cached hash info of a different DistanceType.
		 *	@note Does not check for narrowing conversion of the distance value.
		 */
		template <typename OtherDistanceType>
		constexpr cached_hash_info& operator=(const cached_hash_info<OtherDistanceType, CachedHashType>& other) noexcept
		{
			m_distance = other.get_distance();
			m_cached_hash = other.get_cached_hash();
			return *this;
		}

		/**	Swap operation with a given Robinhood cached hash info.
		 *	@param other The cached hash info with which to swap values.
		 */
		void swap(cached_hash_info& other) noexcept
		{
			using std::swap;
			swap(m_distance, other.m_distance);
			swap(m_cached_hash, other.m_cached_hash);
		}
		/**	Swap two Robinhood cached hash info.
		 *	@param lhs A cached hash info to swap values with rhs.
		 *	@param rhs A cached hash info to swap values with lhs.
		 */
		friend void swap(cached_hash_info& lhs, cached_hash_info& rhs) noexcept
		{
			lhs.swap(rhs);
		}
		/**	Reset this Robinhood info to empty.
		 *	@note Does not clear the cached hash value.
		 */
		constexpr void clear() noexcept
		{
			m_distance = empty_distance_v<distance_type>;
		}
		/**	Increment this Robinhood info's distance by 1.
		 */
		constexpr void push_forward() noexcept
		{
			++m_distance;
		}
		/**	Return true if this Robinhood info indicates an empty position.
		 */
		constexpr bool empty() const noexcept
		{
			return m_distance == empty_distance_v<distance_type>;
		}
		/**	Return the maximum distance value that may be stored in this Robinhood info.
		 *	@return the maximum distance value that may be stored in this Robinhood info.
		 */
		constexpr static distance_type max_distance() noexcept
		{
			return std::numeric_limits<distance_type>::max();
		}
		/**	Return this info's current distance. Will be zero if empty.
		 *	@return this info's current distance. Will be zero if empty.
		 */
		constexpr distance_type get_distance() const noexcept
		{
			return m_distance;
		}
		/**	Return this info's current cached hash. Will be zero if empty.
		 *	@return this info's current cached hash. Will be zero if empty.
		 */
		constexpr cached_hash_type get_cached_hash() const noexcept
		{
			return m_cached_hash;
		}
		/**	Return a copy of this Robinhood info with a distance reduced by 1.
		 *	@return A copy of this Robinhood info with a distance reduced by 1.
		 */
		constexpr cached_hash_info move_back() const noexcept
		{
			SH_ROBINHOOD_ASSERT(m_distance > 1);
			return cached_hash_info(m_distance - 1, m_cached_hash);
		}

		/**	Return true if this Robinhood info's cached hash matches the given info's
		 *	@param other Another cached hash Robinhood info.
		 *	@return Returns true if the two cached hashes are equal.
		 */
		constexpr bool cached_hash_equal(const cached_hash_info& other) const noexcept
		{
			return m_cached_hash == other.get_cached_hash();
		}

	private:
		/**	The Robinhood distance (from expected place) as well as empty condition.
		 */
		distance_type m_distance;
		/**	A cached value to allow hash testing from the info instead of storage memory.
		 */
		cached_hash_type m_cached_hash;
	};

	/**	Specializable template for Robinhood hashtable key/values to indicate how to construct the hashtable info elements.
	 *	@tparam Key A type of key used in the hashtable.
	 *	@tparam ExposedValueType The value_type of the hashtable as visible to users of the container, with an immutable key.
	 *	@tparam MutableValueType The value_type of the hashtable as used internally.
	 *	@tparam SizeType The size_type of the hashtable buckets.
	 */
	template <typename Key, typename ExposedValueType, typename MutableValueType, typename SizeType>
	struct policy final
	{
		/**	The hashtable's key type.
		 */
		using key_type = Key;
		/**	The hashtable's key or key/value pair type as exposed to map/set users.
		 */
		using exposed_value_type = ExposedValueType;
		/**	The hashtable's fully mutable key or key/value pair type used internally.
		 */
		using mutable_value_type = MutableValueType;
		/**	The hashtable's size type (as used for capacity & count).
		 */
		using size_type = SizeType;

		/**	The size to store cached, truncated hash values in Robinhood info elements.
		 *	@details At one byte, will exclude ~99% of mismatching hash values assuming an even spread of hash bits.
		 */
		using cached_hash_type = std::uint8_t;

		/**	The maximal distance type for Robinhood info elements.
		 *	@details Must contain all values from [0, CHAR_BIT*sizeof(key_type) + 1]. The number of key bits gives a
		 *	limit to the true maximum number of keys, and hence, collisions possible.
		 */
		using wide_distance_type = uint_bytes_t<
			ceilui_power_of_two(
				std::min(
					sizeof(key_type) + 1,
					sizeof(size_type)
				)
			)>;

		/**	True if the hash value should be cached in Robinhood info elements.
		 *	@note Specialize cache_hash to easily override this value.
		 */
		constexpr static bool cached_hash = robinhood::cache_hash_v<key_type>;

		/**	True if there is only one width for Robinhood info elements.
		 *	@details This was chosen via experimentation on x86-64 and better
		 *	values may exist for other platforms (or perhaps even on x86-64!)
		 *	and circumstances.
		 */
		constexpr static bool constant_width = std::is_same_v<wide_distance_type, std::uint8_t>
			|| (sizeof(key_type) <= 2 && sizeof(mutable_value_type) <= 4 && sizeof(size_type) <= 4);

		/**	The minimal distance type for Robinhood info elements.
		 *	@note Intended to be the minimum number of bits addressable for memory & cache optimization.
		 */
		using narrow_distance_type = std::conditional_t<constant_width,
			wide_distance_type,
			std::uint8_t>;

		/**	The "wide" info type using wide_distance_type and an optional (if cached_hash is true) cached_hash_type.
		 */
		using wide_info_type = std::conditional_t<cached_hash,
			robinhood::cached_hash_info<wide_distance_type, cached_hash_type>,
			robinhood::info<wide_distance_type>>;

		/**	The "narrow" info type using wide_distance_type and an optional (if cached_hash is true) cached_hash_type.
		 */
		using narrow_info_type = std::conditional_t<cached_hash,
			robinhood::cached_hash_info<narrow_distance_type, cached_hash_type>,
			robinhood::info<narrow_distance_type>>;
	};

	/**	Return the number of bits to shift a hash value to clamp it to a given power-of-two.
	 *	@tparam HashType The type of a hash result that will be shifted by the returned value.
	 *	@tparam SizeType The type of the bucket count given.
	 *	@param bucket_count The maximum number of buckets. Expected to be a power-of-two.
	 *	@return The number of bits to shift a hash value to clamp it to a given power-of-two.
	 */ 
	template <typename HashType, typename SizeType>
	constexpr SizeType shift_bits(const SizeType bucket_count)
	{
		static_assert(sizeof(HashType) >= sizeof(SizeType));
		return sizeof(HashType)*CHAR_BIT - log2ui(bucket_count);
	}

	/**	Return a multiplier for use in a fibonacci hash function.
	 *	@details Returns std::numeric_limits<T>::max() / phi where phi is the golden ratio 0.5*(1 + 5^0.5) ~= 1.618033988749895...
	 *	@note The return value is always rounded to an odd number.
	 *	@tparam T The desired return type and the numerator's source of maximum value.
	 *	@return A multiplier for use in a fibonacci hash function.
	 */ 
	template <typename T> constexpr T fibonacci_fraction();
	template <> constexpr std::uint8_t fibonacci_fraction<std::uint8_t>() { return 157u; }
	template <> constexpr std::uint16_t fibonacci_fraction<std::uint16_t>() { return 40503u; }
	template <> constexpr std::uint32_t fibonacci_fraction<std::uint32_t>() { return 2654435769u; }
	template <> constexpr std::uint64_t fibonacci_fraction<std::uint64_t>() { return 11400714819323198485ull; }

	/**	Process a hash result and clamp it using a value returned by shift_bits.
	 *	@tparam ResultType The type to return. Expected to be a size type.
	 *	@tparam BitsType The type of value returned from shift_bits.
	 *	@param hash A result of a hash function.
	 *	@param shift_bits A value returned by shift_bits.
	 *	@return A processed and clamped hash result.
	 */ 
	template <typename ResultType, typename BitsType>
	constexpr ResultType hash_clamp(ResultType hash, const BitsType shift_bits) noexcept
	{
		// Mix the bits so that only-high-bit type values can contribute to the
		// lower bits and not lose all their precision to overflow below.
		hash ^= hash >> shift_bits;
		// Multiply by a fibonacci fraction value to mix the low-bits into the
		// high-bit positions. Then, clamp (by shifting) to only the lower bits.
		return (fibonacci_fraction<ResultType>() * hash) >> shift_bits;
	}

	/**	Returns given value if zero or power-of-two, otherwise next greater power-of-two.
	 *	@param bucket_count The bucket count from which to round up to the next greatest power of two.
	 *	@return Given value if zero or power-of-two, otherwise next greater power-of-two.
	 */
	template <typename SizeType>
	static constexpr SizeType round_up_bucket_count(const SizeType bucket_count) noexcept
	{
		// zero if zero, power-of-two if power-of-two, otherwise next greater power-of-two
		return ceilui_power_of_two_or_zero(bucket_count);
	}

	// Expected to be a power-of-two by users:
	constexpr std::size_t default_bucket_count = 16;
	static_assert(round_up_bucket_count(default_bucket_count) == default_bucket_count,
		"default_bucket_count expects to be used without rounding.");

	/**	A namespace-scoped is_trivial to allow locally applicable specialization.
	 *	@details Used by implementation to determine if it's safe to memcpy or memset a given type.
	 *	@note Defaults equal to std::is_trivial.
	 */
	template <typename T>
	struct is_trivial final : std::is_trivial<T> { };

	/**	Helper to briefly access value of namespace-scoped is_trivial::value.
	 */
	template <typename T>
	constexpr bool is_trivial_v = is_trivial<T>::value;

	/**	A helper to manage the Robinhood info elements related to hashtable buckets.
	 *	@tparam NarrowInfoType The narrow info type (e.g., policy::narrow_info_type).
	 *	@tparam WideInfoType The wide info type (e.g., policy::wide_info_type).
	 */
	template <typename NarrowInfoType, typename WideInfoType>
	class buckets_info
	{
	public:
		constexpr explicit buckets_info(const bool wide) noexcept
			: m_wide(wide)
		{ }
		buckets_info(const buckets_info&) noexcept = default;
		buckets_info(buckets_info&&) noexcept = default;
		buckets_info& operator=(const buckets_info& other) noexcept
		{
			m_wide = other.m_wide;
			return *this;
		}
		buckets_info& operator=(buckets_info&& other) noexcept
		{
			m_wide = other.m_wide;
			return *this;
		}

		/**	Swap operation with a given bucket info.
		 *	@param other The bucket info info with which to swap values.
		 */
		constexpr void swap(buckets_info& other) noexcept
		{
			using std::swap;
			swap(m_wide, other.m_wide);
		}
		/**	Swap two buckets info.
		 *	@param lhs An info to swap values with rhs.
		 *	@param rhs An info to swap values with lhs.
		 */
		friend void swap(buckets_info& lhs, buckets_info& rhs) noexcept
		{
			lhs.swap(rhs);
		}

	protected:
		/**	False to indicate narrow Robinhood info, true for wide.
		 *	@todo It would be nice to roll this bit into another piece of data.
		 */
		bool m_wide;
	};

	/**	A specialized helper to manage Robinhood info elements related to hashtable buckets when policy::constant_width is true.
	 *	@note Only implements a single info width and is stateless aside from the inherited type.
	 *	@tparam InfoType The info type (e.g., policy::narrow_info_type or policy::wide_info_type).
	 */
	template <typename InfoType>
	class buckets_info<InfoType, InfoType>
	{
	public:
		constexpr explicit buckets_info(const bool wide) noexcept { }
		buckets_info(const buckets_info&) noexcept = default;
		buckets_info(buckets_info&&) noexcept = default;
		buckets_info& operator=(const buckets_info& other) noexcept = default;
		buckets_info& operator=(buckets_info&& other) noexcept = default;

		/**	Swap operation with a given bucket info.
		 *	@note A no-op.
		 *	@param other The bucket info info with which to swap values.
		 */
		constexpr void swap([[maybe_unused]] buckets_info& other) noexcept
		{ }
		/**	Swap two buckets info.
		 *	@param lhs An info to swap values with rhs.
		 *	@param rhs An info to swap values with lhs.
		 */
		friend void swap(buckets_info& lhs, buckets_info& rhs) noexcept
		{
			lhs.swap(rhs);
		}
	};

	/**	A sized container of keys and any values, associated Robinhood info, an element count, and allocator.
	 *	@tparam PolicyType The Robinhood hashtable policy type.
	 *	@tparam Allocator An std::allocator-like class.
	 */
	template <typename PolicyType, typename Allocator>
	class buckets
		// To take advantage of [[no_unique_address]] features of inheritance:
		: private buckets_info<
			typename PolicyType::narrow_info_type,
			typename PolicyType::wide_info_type>
		, private std::allocator_traits<Allocator>::template rebind_alloc<typename PolicyType::mutable_value_type>
	{
	public:
		using policy_type = PolicyType;
		using size_type = typename policy_type::size_type;
		using value_type = typename policy_type::mutable_value_type;
		using narrow_info_type = typename policy_type::narrow_info_type;
		using wide_info_type = typename policy_type::wide_info_type;
		using buckets_info_type = buckets_info<narrow_info_type, wide_info_type>;
		using allocator_traits = std::allocator_traits<Allocator>;
		using allocator_type = typename allocator_traits::template rebind_alloc<value_type>;
		using info_pointer = void*;
		using const_info_pointer = const void*;
		using pointer = value_type*;
		using const_pointer = const value_type*;
		using iterator = pointer;
		using const_iterator = const_pointer;

		static_assert(std::is_unsigned_v<size_type>, "size_type required to be unsigned integral.");

		/**	The number of Robinhood info items in addition to m_capacity in m_info.
		 */
		constexpr static size_type info_tail = robinhood::info_tail_v<size_type>;

		/**	Minimal constructor for zero capacity.
		 */
		constexpr explicit buckets(const allocator_type& alloc) noexcept
			: buckets_info_type(/* wide: */ false)
			, allocator_type(alloc)
			, m_info(&empty_info<narrow_info_type>())
			, m_data(nullptr)
			, m_capacity(0)
			, m_count(0)
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			, m_generation(initialize_generation())
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
		{ }
		/**	Constructor for a given capacity and narrow width.
		 *	@param capacity The capacity of values.
		 */
		constexpr explicit buckets(const size_type capacity, const allocator_type& alloc)
			: buckets_info_type(/* wide: */ false)
			, allocator_type(alloc)
			, m_info(&empty_info<narrow_info_type>())
			, m_data(nullptr)
			, m_capacity(capacity)
			, m_count(0)
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			, m_generation(initialize_generation())
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
		{
			// When m_capacity is zero, m_info should be empty_info<info_type> and m_data, nullptr.
			if (m_capacity > 0)
			{
				auto narrow_info_alloc = get_info_allocator<narrow_info_type>();
				m_info = rebind_traits<narrow_info_type>::allocate(narrow_info_alloc, capacity + info_tail);
				clear_info(static_cast<narrow_info_type*>(m_info), capacity + info_tail);
				m_data = rebind_traits<value_type>::allocate(get_allocator(), m_capacity);
			}
		}
		/**	Constructor for a given capacity and width.
		 *	@param capacity The capacity of values.
		 *	@param wide False for narrow info, true for wide.
		 */
		constexpr buckets(const size_type capacity, const bool wide, const allocator_type& alloc)
			: buckets_info_type(wide)
			, allocator_type(alloc)
			, m_info(nullptr)
			, m_data(nullptr)
			, m_capacity(capacity)
			, m_count(0)
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			, m_generation(initialize_generation())
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
		{
			// When m_capacity is zero, m_info should be empty_info<info_type> and m_data, nullptr.
			if (m_capacity > 0)
			{
				with_info([this](const auto* const info)
				{
					using info_type = std::decay_t<decltype(*info)>;
					auto info_alloc = get_info_allocator<info_type>();
					m_info = rebind_traits<info_type>::allocate(info_alloc, m_capacity + info_tail);
					clear_info(static_cast<info_type*>(m_info), m_capacity + info_tail);
				});
				m_data = rebind_traits<value_type>::allocate(get_allocator(), m_capacity);
			}
			else
			{
				with_info([this](const auto* const info) noexcept
				{
					using info_type = std::decay_t<decltype(*info)>;
					m_info = &empty_info<info_type>();
				});
			}
		}
		/**	Copy constructor from another buckets plus specified allocator.
		 *	@param other The other buckets to copy into this one.
		 *	@param alloc The allocator to assume for this buckets.
		 */
		buckets(const buckets& other, const allocator_type& alloc)
			: buckets_info_type(static_cast<const buckets_info_type&>(other))
			, allocator_type(alloc)
			, m_info(other.m_info)
			, m_data(nullptr)
			, m_capacity(other.m_capacity)
			, m_count(other.m_count)
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			, m_generation(initialize_generation())
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
		{
			// When m_capacity is zero, m_info should be empty_info<info_type> and m_data, nullptr.
			if (m_capacity > 0)
			{
				allocate_and_initialize_one_or_more_values_by_copy(other);
			}
		}
		/**	Copy constructor from another buckets.
		 *	@note Uses select_on_container_copy_construction to copy allocator from other.
		 *	@param other The other buckets to copy into this one.
		 *	@param alloc The allocator to assume for this buckets.
		 */
		buckets(const buckets& other)
			: buckets(other, rebind_traits<value_type>::select_on_container_copy_construction(other.get_allocator()))
		{ }
		/**	Move constructor from another buckets with an is_always_equal specified allocator.
		 *	@param other The other buckets to move into this one.
		 *	@param alloc The allocator to assume for this buckets.
		 */
		buckets(buckets&& other, const allocator_type& alloc, std::true_type) noexcept
			: buckets_info_type(std::exchange(static_cast<buckets_info_type&>(other), buckets_info_type(false)))
			, allocator_type(std::move(other.get_allocator()))
			, m_info(std::exchange(other.m_info, &empty_info<narrow_info_type>()))
			, m_data(std::exchange(other.m_data, nullptr))
			, m_capacity(std::exchange(other.m_capacity, 0))
			, m_count(std::exchange(other.m_count, 0))
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			, m_generation(initialize_generation())
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
		{
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			// Always invalidate source iterators on move construction.
			other.invalidate_iterators();
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
		}
		/**	Move constructor from another buckets with a possibly non-is_always_equal specified allocator.
		 *	@param other The other buckets to move into this one.
		 *	@param alloc The allocator to assume for this buckets.
		 */
		buckets(buckets&& other, const allocator_type& alloc, std::false_type)
			: buckets_info_type(static_cast<const buckets_info_type&>(other))
			, allocator_type(alloc)
			, m_info(other.m_info)
			, m_data(nullptr)
			, m_capacity(other.m_capacity)
			, m_count(other.m_count)
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			, m_generation(initialize_generation())
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
		{
			if (alloc == other.get_allocator())
			{
				// Allocators are infact compatible and an exchange will suffice.
				other.buckets_info_type::operator=(buckets_info_type(false));
				m_info = std::exchange(other.m_info, &empty_info<narrow_info_type>());
				m_data = std::exchange(other.m_data, nullptr);
				other.m_capacity = 0;
				other.m_count = 0;
			}
			else if (m_capacity > 0)
			{
				// Allocators are incompatible, construct new space and move the values.
				allocate_and_initialize_one_or_more_values_by_move(other);
				// Capacity and count were already copied prior to allocate_and_initialize_one_or_more_values_by_move.
			}
			/*
			else
			{
				// Capacity is zero, m_info should remain empty_info<info_type> and m_data, nullptr.
			}
			*/
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			// Always invalidate source iterators on move construction.
			other.invalidate_iterators();
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
		}
		/**	Move constructor from another buckets with specified allocator.
		 *	@param other The other buckets to move into this one.
		 *	@param alloc The allocator to assume for this buckets.
		 */
		buckets(buckets&& other, const allocator_type& alloc)
			noexcept(typename allocator_traits::is_always_equal())
			: buckets(std::move(other), alloc, std::bool_constant<(typename allocator_traits::is_always_equal())>{})
		{ }
		/**	Move constructor from another buckets.
		 *	@note Uses select_on_container_copy_construction to copy allocator from other.
		 *	@param other The other buckets to move into this one.
		 */
		constexpr buckets(buckets&& other)
			noexcept(typename allocator_traits::is_always_equal())
			: buckets(std::move(other), rebind_traits<value_type>::select_on_container_copy_construction(other.get_allocator()))
		{ }

		/**	Destructor.
		 *	@note If m_count is zero, will skip iterating the info & destructing values.
		 */
		~buckets() noexcept
		{
			destruct_and_deallocate_values_and_info();
		}

		/**	Copy assignment operator for another buckets.
		 *	@details Destroys any current contents of this bucket.
		 *	@param other The other buckets to copy info & values into this one.
		 *	@return This.
		 */
		buckets& operator=(const buckets& other)
		{
			const auto do_swap = [this](buckets& other) noexcept
			{
				using std::swap;
				// Swap width.
				this->buckets_info_type::swap(static_cast<buckets_info_type&>(other));
				// Swap other data members.
				swap(m_info, other.m_info);
				swap(m_data, other.m_data);
				swap(m_capacity, other.m_capacity);
				swap(m_count, other.m_count);
			};
			// Do a resize if necessary and copy data without needing to propagate the allocators.
			const auto do_resize_if_necessary_and_copy = [&](const buckets& other)
			{
				if (std::is_nothrow_copy_constructible_v<value_type> && m_capacity == other.m_capacity && get_wide() == other.get_wide())
				{
					// Capacity & width are equal & there's no desire to propagate allocator. Copy values in-place.
					copy_replace_values_from_same_sized_buckets(other);
					// Finally, copy the value of m_count.
					m_count = other.m_count;
				}
				else
				{
					// Copy into temporary using this allocator. Will allocate (could throw) and afterwards copy values.
					buckets new_buckets(other, get_allocator());
					// Swap, leaving new_buckets to destruct our previous values.
					do_swap(new_buckets);
				}
			};
			if constexpr (typename allocator_traits::is_always_equal())
			{
				do_resize_if_necessary_and_copy(other);
			}
			else if (this->allocator_type::operator==(static_cast<const allocator_type&>(other))
				|| false == typename allocator_traits::propagate_on_container_copy_assignment())
			{
				do_resize_if_necessary_and_copy(other);
			}
			else
			{
				// Copy into temporary using other allocator. Will allocate (could throw) and afterwards copy values.
				buckets new_buckets(other);
				// Swap, leaving new_buckets to destruct our previous values.
				using std::swap;
				swap(get_allocator(), new_buckets.get_allocator());
				do_swap(new_buckets);
			}
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			// Always invalidate destination iterators on copy assignment.
			invalidate_iterators();
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
			return *this;
		}

		/**	Move assignment operator from another buckets.
		 *	@details If the other bucket can be moved-from, it may be reset to narrow.
		 *	@param other The other buckets to move into this one.
		 *	@return This.
		 */
		buckets& operator=(buckets&& other)
			noexcept(typename allocator_traits::is_always_equal())
		{
			const auto do_exchange = [this](buckets& other) noexcept
			{
				// Exchange wide.
				constexpr bool wide = false;
				this->buckets_info_type::operator=(std::exchange(static_cast<buckets_info_type&>(other), buckets_info_type(wide)));
				// Exchange other data members.
				m_info = std::exchange(other.m_info, &empty_info<narrow_info_type>());
				m_data = std::exchange(other.m_data, nullptr);
				m_capacity = std::exchange(other.m_capacity, 0);
				m_count = std::exchange(other.m_count, 0);
			};
			const auto do_swap = [this](buckets& other) noexcept
			{
				using std::swap;
				// Swap wide.
				this->buckets_info_type::swap(static_cast<buckets_info_type&>(other));
				// Swap other data members.
				swap(m_info, other.m_info);
				swap(m_data, other.m_data);
				swap(m_capacity, other.m_capacity);
				swap(m_count, other.m_count);
			};
			if constexpr (typename allocator_traits::is_always_equal())
			{
				destruct_and_deallocate_values_and_info();
				do_exchange(other);
			}
			else if (this->allocator_type::operator==(static_cast<const allocator_type&>(other)))
			{
				destruct_and_deallocate_values_and_info();
				do_exchange(other);
			}
			else if (typename allocator_traits::propagate_on_container_move_assignment())
			{
				// Move into temporary using other allocator. Will allocate (could throw) and afterwards move_if_noexcept values.
				buckets new_buckets(std::move(other));
				// Swap, leaving new_buckets to destruct our previous values.
				using std::swap;
				swap(get_allocator(), new_buckets.get_allocator());
				do_swap(new_buckets);
			}
			else if (m_capacity != other.m_capacity || get_wide() != other.get_wide())
			{
				// Move into temporary using this allocator. Will allocate (could throw) and afterwards move_if_noexcept values.
				buckets new_buckets(std::move(other), get_allocator());
				// Swap, leaving new_buckets to destruct our previous values.
				do_swap(new_buckets);
			}
			else
			{
				// Capacity & width are equal & there's no desire to propagate allocator. Move values in-place.
				move_replace_values_from_same_sized_buckets(other);
				// Finally, copy the value of m_count.
				m_count = other.m_count;
			}
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			// Always invalidate source & destination iterators on move assignment.
			invalidate_iterators();
			other.invalidate_iterators();
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
			return *this;
		}

		/**	Swap contents with another buckets.
		 *	@param other The other buckets to move into this one.
		 */
		void swap(buckets& other)
			noexcept(std::is_nothrow_swappable_v<buckets_info_type>
				&& std::is_nothrow_swappable_v<allocator_type>
				&& typename allocator_traits::is_always_equal())
		{
			const auto do_swap = [this](buckets& lhs, buckets& rhs) noexcept
			{
				using std::swap;
				// Swap width.
				lhs.buckets_info_type::swap(static_cast<buckets_info_type&>(rhs));
				// Swap other data members.
				swap(lhs.m_info, rhs.m_info);
				swap(lhs.m_data, rhs.m_data);
				swap(lhs.m_capacity, rhs.m_capacity);
				swap(lhs.m_count, rhs.m_count);
			};
			if constexpr (typename allocator_traits::is_always_equal())
			{
				do_swap(*this, other);
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
				// Swap iterator generation for stateless allocators.
				using std::swap;
				swap(m_generation, other.m_generation);
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
			}
			else if (this->allocator_type::operator==(static_cast<const allocator_type&>(other)))
			{
				do_swap(*this, other);
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
				// Swap iterator generation for equivalent allocators.
				using std::swap;
				swap(m_generation, other.m_generation);
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
			}
			else if (typename allocator_traits::propagate_on_container_swap())
			{
				using std::swap;
				swap(get_allocator(), other.get_allocator());
				do_swap(*this, other);
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
				// Swap iterator generation for swap-propagated allocators.
				swap(m_generation, other.m_generation);
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
			}
			else if constexpr (is_trivial_v<value_type>)
			{
				// If trivial copying is accessible or we cannot rely on move_if_noexcept to be noexcept, perform a full copy:
				buckets these_buckets_other_allocator(*this, other.get_allocator());
				buckets other_buckets_this_allocator(other, get_allocator());

				// Swap into place, leaving temporaries to destruct:
				do_swap(other, these_buckets_other_allocator);
				do_swap(*this, other_buckets_this_allocator);

#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
				// Invalidate source & destination iterators on swap with stateful, non-equivalent, and non-swap-propagated allocators.
				invalidate_iterators();
				other.invalidate_iterators();
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
			}
			else
			{
				// Otherwise try to move the values into the temporaries. Perform allocation first as could throw:
				buckets these_buckets_other_allocator(m_capacity, get_wide(), other.get_allocator());
				buckets other_buckets_this_allocator(other.m_capacity, other.get_wide(), get_allocator());
				// Move-initialize values:
				if (m_count > 0)
				{
					with_info([this, &these_buckets_other_allocator](const auto* const info) noexcept
					{
						these_buckets_other_allocator.initialize_values_by_move(*this, info);
					});
					these_buckets_other_allocator.m_count = m_count;
				}
				if (other_buckets_this_allocator.m_count > 0)
				{
					other.with_info([&other, &other_buckets_this_allocator](const auto* const other_info) noexcept
					{
						other_buckets_this_allocator.initialize_values_by_move(other, other_info);
					});
					other_buckets_this_allocator.m_count = other.m_count;
				}
				// Swap into place, leaving temporaries to destruct:
				do_swap(other, these_buckets_other_allocator);
				do_swap(*this, other_buckets_this_allocator);

#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
				// Invalidate source & destination iterators on swap with stateful, non-equivalent, and non-swap-propagated allocators.
				invalidate_iterators();
				other.invalidate_iterators();
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
			}
		}
		friend void swap(buckets& lhs, buckets& rhs)
			noexcept(noexcept(std::declval<buckets&>().swap(std::declval<buckets&>())))
		{
			lhs.swap(rhs);
		}

		/**	Return false if Robinhood info elements are stored in narrow_info_type.
		 *	@return False if Robinhood info elements are stored in narrow_info_type.
		 */
		constexpr bool get_wide() const noexcept
		{
			if constexpr (policy_type::constant_width)
			{
				return false;
			}
			else
			{
				return this->m_wide;
			}
		}
		/**	Returns the current capacity for values.
		 *	@return The current capacity for values.
		 */
		constexpr size_type get_capacity() const noexcept
		{
			return m_capacity;
		}
		/**	Returns the current count of constructed values.
		 *	@return The current count of constructed values.
		 */
		constexpr size_type get_count() const noexcept
		{
			return m_count;
		}

		/**	Constructs a value at the given index.
		 *	@note Only asserts that index within bounds and currently destructed.
		 *	@param info A pointer to the current (m_capacity + info_tail) element array of Robinhood info elements.
		 *	@param index The presently destructed index to construct with args.
		 *	@param info_value The Robinhood info pertaining to the new index & value.
		 *	@param args The arguments with which to construct the new value_type.
		 *	@tparam InfoType The type of Robinhood info elements in the info element array.
		 */
		template <typename InfoType, typename... Args>
		void emplace(InfoType* const info, const size_type index, const InfoType& info_value, Args&&... args)
			noexcept(noexcept(this->construct_value(size_type{}, args...)))
		{
			SH_ROBINHOOD_ASSERT(index < m_capacity);
			SH_ROBINHOOD_ASSERT(empty(info, index) == true);
			info[index] = info_value;
			construct_value(index, std::forward<Args>(args)...);
			++m_count;
			// Iterators invalidation for emplace handled in do_emplace.
		}

		/**	Destruct the value at a given index & marks the info as empty.
		 *	@note Only asserts that index within bounds and currently constructed.
		 *	@param info A pointer to the current (m_capacity + info_tail) element array of Robinhood info elements.
		 *	@param index The presently constructed index to destruct.
		 *	@tparam InfoType The type of Robinhood info elements in the info element array.
		 */
		template <typename InfoType>
		void erase(InfoType* const info, const size_type index) noexcept
		{
			SH_ROBINHOOD_ASSERT(index < m_capacity);
			SH_ROBINHOOD_ASSERT(empty(info, index) == false);
			destroy_value(index);
			info[index].clear();
			--m_count;
			// Iterators invalidation for erase handled in do_erase.
		}

		/**	Destruct all constructed values.
		 */
		void clear() noexcept
		{
			// If m_count == 0, there are no values to destruct.
			if (m_count > 0)
			{
				with_info([this](auto* const info) noexcept
				{
					// If value_type is trivially destructible, clear info in bulk..
					if constexpr (std::is_trivially_destructible_v<value_type>)
					{
						clear_info(info, m_capacity); // no + info_tail
					}
					else
					{
						destruct_values_and_clear_info(info);
					}
				});
				m_count = 0;
			}
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			// Invalidate iterators on clear.
			invalidate_iterators();
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
		}

		/**	Calls func with a pointer to constant Robinhood info elements.
		 *	@note If m_info is altered during the runtime of func, the info parameter may become outdated.
		 *	@return The return value from func.
		 */
		template <typename F>
		constexpr auto with_info(F&& func)
			noexcept(noexcept(func(std::declval<narrow_info_type*>())))
		{
			if constexpr (policy_type::constant_width)
			{
				return func(get_info<narrow_info_type>());
			}
			else
			{
				// Optimistcally assume our info is narrow.
				if SH_ROBINHOOD_UNLIKELY(this->m_wide)
				{
					return func(get_info<wide_info_type>());
				}
				else
				{
					return func(get_info<narrow_info_type>());
				}
			}
		}
		/**	Calls func with a pointer to constant Robinhood info elements.
		 *	@note If m_info is altered during the runtime of func, the info parameter may become outdated.
		 *	@return The return value from func.
		 */
		template <typename F>
		constexpr auto with_info(F&& func) const
			noexcept(noexcept(func(std::declval<const narrow_info_type*>())))
		{
			if constexpr (policy_type::constant_width)
			{
				return func(get_info<narrow_info_type>());
			}
			else
			{
				// Optimistcally assume our info is narrow.
				if SH_ROBINHOOD_UNLIKELY(this->m_wide)
				{
					return func(get_info<wide_info_type>());
				}
				else
				{
					return func(get_info<narrow_info_type>());
				}
			}
		}
		/**	Return a pointer to mutable Robinhood info elements.
		 *	@note Only asserts that the Robinhood info elements are in-fact stored as InfoType.
		 *	@return A pointer to mutable InfoType Robinhood info elements.
		 *	@tparam InfoType The type of Robinhood info elements in the info element array.
		 */
		template <typename InfoType>
		constexpr InfoType* get_info() noexcept
		{
			SH_ROBINHOOD_ASSERT((get_wide() == false || std::is_same_v<InfoType, wide_info_type> || std::is_same_v<InfoType, void>));
			SH_ROBINHOOD_ASSERT((get_wide() == true || std::is_same_v<InfoType, narrow_info_type> || std::is_same_v<InfoType, void>));
			return static_cast<InfoType*>(m_info);
		}
		/**	Return a pointer to mutable Robinhood info elements.
		 *	@note Only asserts that the Robinhood info elements are in-fact stored as InfoType.
		 *	@return A pointer to mutable InfoType Robinhood info elements.
		 *	@tparam InfoType The type of Robinhood info elements in the info element array.
		 */
		template <typename InfoType>
		constexpr const InfoType* get_info() const noexcept
		{
			SH_ROBINHOOD_ASSERT((get_wide() == false || std::is_same_v<InfoType, wide_info_type> || std::is_same_v<InfoType, void>));
			SH_ROBINHOOD_ASSERT((get_wide() == true || std::is_same_v<InfoType, narrow_info_type> || std::is_same_v<InfoType, void>));
			return static_cast<const InfoType*>(m_info);
		}

		/**	Alters the Robinhood info elements from narrow_info_type to wide_info_type.
		 *	@note Only asserts that the Robinhood info wide is in-fact narrow. Not expected to be called when empty (m_count or m_capacity of 0).
		 */
		void widen()
		{
			static_assert(policy_type::constant_width == false);
			SH_ROBINHOOD_ASSERT(get_wide() == false);
			SH_ROBINHOOD_ASSERT(m_count > 0);
			auto narrow_info_alloc = get_info_allocator<narrow_info_type>();
			auto wide_info_alloc = get_info_allocator<wide_info_type>();
			auto* const narrow_info = get_info<narrow_info_type>();
			auto* const wide_info = rebind_traits<wide_info_type>::allocate(wide_info_alloc, m_capacity + info_tail);
			std::copy(narrow_info, narrow_info + (m_capacity + info_tail), wide_info);
			rebind_traits<narrow_info_type>::deallocate(narrow_info_alloc, narrow_info, m_capacity + info_tail);
			set_wide(true);
			m_info = wide_info;
		}

		/**	Get a mutable value_type reference by index.
		 *	@note Only asserts that indices are within bounds & currently constructed.
		 *	@param index The presently constructed index to return a value_type reference.
		 *	@return A mutable reference to a value_type at index.
		 */
		value_type& get_value(const size_type index) noexcept
		{
			SH_ROBINHOOD_ASSERT(index < m_capacity);
			SH_ROBINHOOD_ASSERT(get_wide() == true || get_info<narrow_info_type>()[index].empty() == false);
			SH_ROBINHOOD_ASSERT(get_wide() == false || get_info<wide_info_type>()[index].empty() == false);
			return *get_value_address(index);
		}
		/**	Get a constant value_type reference by index.
		 *	@note Only asserts that indices are within bounds & currently constructed.
		 *	@param index The presently constructed index to return a value_type reference.
		 *	@return A constant reference to a value_type at index.
		 */
		const value_type& get_value(const size_type index) const noexcept
		{
			SH_ROBINHOOD_ASSERT(index < m_capacity);
			SH_ROBINHOOD_ASSERT(get_wide() == true || get_info<narrow_info_type>()[index].empty() == false);
			SH_ROBINHOOD_ASSERT(get_wide() == false || get_info<wide_info_type>()[index].empty() == false);
			return *get_value_address(index);
		}

		/**	Swaps & decreases the distance of an info & value to an earlier (closer to its hash_clamp) index.
		 *	@note Does not check to see if the indices are in bounds. Only asserts that they're destructed & constructed, respectively.
		 *	@param info A pointer to the current (m_capacity + info_tail) element array of Robinhood info elements.
		 *	@param former_index The presently destructed index to which the info & value will move.
		 *	@param current_index The presently constructed index from which the info & value will move.
		 *	@return The address of a value.
		 *	@tparam InfoType The type of Robinhood info elements in the info element array.
		 */
		template <typename InfoType>
		void move_back(InfoType* const info, const size_type former_index, const size_type current_index) noexcept
		{
			SH_ROBINHOOD_ASSERT(former_index < m_capacity);
			SH_ROBINHOOD_ASSERT(empty(info, former_index) == true);
			SH_ROBINHOOD_ASSERT(empty(info, current_index) == false);
			static_assert(std::is_nothrow_move_constructible_v<value_type>, "value_type must be nothrow move constructible.");
			construct_value(former_index, std::move(get_value(current_index)));
			static_assert(std::is_nothrow_copy_assignable_v<InfoType>, "InfoType excepted to have noexcept copy assignment operation.");
			info[former_index] = info[current_index].move_back();
			destroy_value(current_index);
			info[current_index].clear();
		}

		/**	Return the address of a value by index in a minimally-safe fashion.
		 *	@note Only asserts that the index is in bounds and allows accessing one-past for iterator values. Does not check that it's currently constructed.
		 *	@param index The index of the value to return.
		 *	@return The address of a value.
		 */
		constexpr pointer get_value_address(const size_type index) noexcept
		{
			SH_ROBINHOOD_ASSERT(index <= m_capacity);
			return m_data + index;
		}
		/**	Return the address of a value by index in a minimally-safe fashion.
		 *	@note Only asserts that the index is in bounds and allows accessing one-past for iterator values. Does not check that it's currently constructed.
		 *	@param index The index of the value to return.
		 *	@return The address of a value.
		 */
		constexpr const_pointer get_value_address(const size_type index) const noexcept
		{
			SH_ROBINHOOD_ASSERT(index <= m_capacity);
			return m_data + index;
		}

		/**	Destruct the given value without updating info or m_count.
		 *	@note Only asserts that the value is within m_data.
		 *	@param v A reference to the value to destruct.
		 */
		void destroy(value_type& v) noexcept
		{
			SH_ROBINHOOD_ASSERT(&v >= get_value_address(0) && &v < get_value_address(m_capacity));
			rebind_traits<value_type>::destroy(get_allocator(), &v);
		}
		/**	If the caller has called destroy_value on all values and these buckets are to be destructed, mark count as zero to prevent re-iterating (and double destroying!) the buckets.
		 */
		void mark_count_zero_to_indicate_all_destroyed() noexcept
		{
			m_count = 0;
		}

		/**	Return the value_type allocator.
		 *	@return The value_type allocator.
		 */
		constexpr allocator_type& get_allocator() noexcept
		{
			return static_cast<allocator_type&>(*this);
		}

		/**	Return the value_type allocator.
		 *	@return The value_type allocator.
		 */
		constexpr const allocator_type& get_allocator() const noexcept
		{
			return static_cast<const allocator_type&>(*this);
		}

#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
		/**	Return an initial value for a debug iterator generation.
		 *	@details Not required to always return the same value.
		 *	@return An initial value for a debug iterator generation.
		 */
		constexpr static size_type initialize_generation() noexcept
		{
			// Could use a pseudo-random number to initialize.
			return 0;
		}

		/**	Invalidate the current the debug iterator generation.
		 *	@details Not required to always increment by one.
		 */
		void invalidate_iterators() noexcept
		{
			// Could increment by a non-one value to prevent intersection.
			++m_generation;
		}

		/**	Check the validity of a given iterator debugging info.
		 *	@param index The index of the iteration supplying debug.
		 *	@param debug The iterator debug info.
		 *	@return True if the given iterator debugging info is valid.
		 */
		constexpr bool check_iterator_debug(const size_type index, const iterator_debug<size_type>& debug) const noexcept
		{
			return m_capacity == debug.m_capacity
				// end() is given a pass on matching m_generation (as long as capacity matched above).
				&& (index == m_capacity || m_generation == debug.m_generation);
		}

		/**	Return the iterator debugging info.
		 *	@return The iterator debugging info.
		 */
		constexpr iterator_debug<size_type> get_iterator_debug() const noexcept
		{
			return iterator_debug<size_type>{ m_generation, m_capacity };
		}
#endif // SH_ROBINHOOD_DEBUG_ITERATOR

	private:
		/**	Set if Robinhood info elements are stored in narrow_info_type (false) or wide_info_type (true).
		 *	@param wide False if Robinhood info elements are stored in narrow_info_type, true if stored in wide_info_type.
		 */
		void set_wide(const bool wide) noexcept
		{
			if constexpr (policy_type::constant_width)
			{
				SH_ROBINHOOD_ASSERT(wide == false);
			}
			else
			{
				this->m_wide = wide;
			}
		}

		template <typename T>
		using rebind_alloc = typename allocator_traits::template rebind_alloc<T>;
		template <typename T>
		using rebind_traits = typename allocator_traits::template rebind_traits<T>;

		/**	Return the requested info type allocator.
		 *	@return An info allocator.
		 *	@tparam InfoType The resulting allocator's value_type. Must be narrow_info_type or wide_info_type.
		 */
		template <typename InfoType>
		constexpr rebind_alloc<InfoType> get_info_allocator() const noexcept
		{
			static_assert(std::is_same_v<InfoType, narrow_info_type> || std::is_same_v<InfoType, wide_info_type>,
				"Info allocator expected to be for either narrow or wide info type.");
			return rebind_alloc<InfoType>(get_allocator());
		}

		/**	Helper to access a single, static Robinhood info of the given type.
		 *	@note Returned reference should not be changed.
		 *	@return A reference to a single, static Robinhood info.
		 *	@tparam InfoType The empty InfoType to return.
		 */
		template <typename InfoType>
		static InfoType& empty_info() noexcept
		{
			static_assert(info_tail > 0, "empty_info has no purpose if info_tail is zero.");
			static_assert(info_tail <= 1, "empty_info's single element is insufficient if info_tail > 1.");
			constexpr auto distance = empty_distance_v<typename InfoType::distance_type>;
			constexpr std::size_t cached_hash = 0;
			static InfoType static_empty_info(distance, cached_hash);
			SH_ROBINHOOD_ASSERT(static_empty_info.empty());
			return static_empty_info;
		}

		/**	Construct the value at the given index without updating info.
		 *	@note Does not check to see if the index is in bounds or that the value is currently destructed.
		 *	@param index The index of the value to construct.
		 *	@param args The arguments to forward to the value_type constructor.
		 *	@tparam Args The types of the given arguments.
		 */
		template <typename... Args>
		void construct_value(const size_type index, Args&&... args)
			noexcept(std::is_nothrow_constructible_v<value_type, Args...>)
		{
			rebind_traits<value_type>::construct(
				get_allocator(),
				get_value_address(index),
				std::forward<Args>(args)...);
		}

		/**	Destruct the value at the given index without updating info or m_count.
		 *	@note Only asserts that the index is in bounds (via empty) & currently constructed.
		 *	@param index The index of the value to destruct.
		 */
		void destroy_value(const size_type index) noexcept
		{
			SH_ROBINHOOD_ASSERT(index < m_capacity);
			SH_ROBINHOOD_ASSERT(empty(index) == false);
			rebind_traits<value_type>::destroy(get_allocator(), get_value_address(index));
		}

		/**	Bulk copy info values.
		 *	A thin wrapper around std::memcpy.
		 *	@param dst The copy destination, which is an array of "n" InfoType elements.
		 *	@param src The copy source , which is an array of "n" (already initialized) InfoType elements.
		 *	@param n The number of InfoType elements in both dst and src.
		 *	@tparam InfoType The type of the given source & destination Robinhood info elements.
		 */
		template <typename InfoType>
		static void copy_trivial_info(InfoType* const dst, const InfoType* const src, const size_type n) noexcept
		{
			static_assert(is_trivial_v<InfoType>, "InfoType required to be trivial in order to be memcpy-able.");
			static_assert(std::is_same_v<InfoType, narrow_info_type> || std::is_same_v<InfoType, wide_info_type>, "Is this actually an InfoType?");
			std::memcpy(dst, src, sizeof(InfoType) * n);
		}
		/**	Bulk clear info values.
		 *	A thin wrapper around std::memset.
		 *	@param info The clear destination, which is an array of "n" InfoType elements.
		 *	@param n The number of InfoType elements in info.
		 *	@tparam InfoType The type of the given Robinhood info elements.
		 */
		template <typename InfoType>
		static void clear_info(InfoType* const info, const size_type n) noexcept
		{
			static_assert(is_trivial_v<InfoType>, "InfoType required to be trivial in order to be memset-able.");
			static_assert(std::is_same_v<InfoType, narrow_info_type> || std::is_same_v<InfoType, wide_info_type>, "Is this actually an InfoType?");
			constexpr int clear_value = empty_distance_v<int>;
			static_assert(clear_value == empty_distance_v<typename InfoType::distance_type>, "Comparable empty_distance_v required.");
			std::memset(info, clear_value, sizeof(InfoType) * n);
		}
		/**	Bulk copy value_type elements which may contain value_type.
		 *	A thin wrapper around std::memcpy.
		 *	@param dst The copy destination, which is an array of "n" value_type elements.
		 *	@param src The copy source , which is an array of "n" (possibly initialized) value_type elements.
		 *	@param n The number of value_type elements in both dst and src.
		 */
		static void copy_trivial_values(value_type* const dst, const value_type* const src, const size_type n) noexcept
		{
			static_assert(is_trivial_v<value_type>, "ValueType required to be trivial in order to be memcpy-able.");
			std::memcpy(dst, src, sizeof(value_type) * n);
		}

		/**	Initialize one or more values from given buckets.
		 *	@note Expects other's m_count to be greater than zero. This and other must share the same width and allocated capacity. Does not destroy or check existing data or info in this prior to copying/moving. Cannot be relied upon to always set m_count or m_capacity.
		 *	@param other The source of values to copy or move.
		 *	@param copy_or_move The adapter function to cast the value to the type from which to construct.
		 *	@tparam Buckets Either buckets or const buckets.
		 *	@tparam InfoType Other's info type.
		 *	@tparam CopyOrMove Accepts the copied value and can return it either by reference or as a move-reference.
		 */
		template <typename BucketsType, typename InfoType, typename CopyOrMove>
		void initialize_values(BucketsType& other, const InfoType* const other_info, CopyOrMove&& copy_or_move)
			noexcept(noexcept(copy_or_move(std::declval<value_type&>())))
		{
			// Assume that other is non-empty and SH_ROBINHOOD_ASSERT that is true.
			SH_ROBINHOOD_ASSERT(other.m_count > 0);
			// Check that destination data are allocated.
			SH_ROBINHOOD_ASSERT(m_data != nullptr);
			SH_ROBINHOOD_ASSERT(m_info != &empty_info<narrow_info_type>() && m_info != &empty_info<wide_info_type>());
			// Will SH_ROBINHOOD_ASSERT that info is of the appropriate type: get_wide() must match.
			InfoType* const info = get_info<InfoType>();
			if constexpr (is_trivial_v<value_type>)
			{
				// value_type is trivial, copy info & data in bulk.
				copy_trivial_info(info, other_info, other.m_capacity + info_tail);
				copy_trivial_values(m_data, other.m_data, other.m_capacity);
			}
			else
			{
#ifndef NDEBUG
				// To quiet the SH_ROBINHOOD_ASSERT in construct_value below, set the capacity early.
				m_capacity = other.m_capacity;
#endif // !NDEBUG
				// Copy & construct [0, info_tail):
				for (size_type index = 0; index < other.m_capacity; ++index)
				{
					// As our data is newly allocated, there are no values to destroy_value.
					if (other_info[index].empty() == false)
					{
#ifndef NDEBUG
						// Info is undefined, clear it for the asserts in construct_value.
						info[index].clear();
#endif // !NDEBUG
						construct_value(index, copy_or_move(other.get_value(index)));
					}
					// Outside of the conditional to allow additional optimization.
					info[index] = other_info[index];
				}
				// Copy the info_tail:
				info[other.m_capacity] = other_info[other.m_capacity];
			}
			// Does not always set m_count or m_capacity!
		}
		/**	Initializes values by move_if_noexcept from given buckets.
		 *	@param other The source of values to move.
		 *	@tparam InfoType Other's info type.
		 */
		template <typename InfoType>
		void initialize_values_by_move(buckets& other, const InfoType* const other_info)
		{
			initialize_values(other, other_info, [](value_type& value) noexcept { return std::move_if_noexcept(value); });
		}

		/**	Allocate bucket space and initialize one or more possible values from given buckets.
		 *	@note Requires other's m_capacity to be greater than zero. Does not destroy or check existing data or info in this prior to allocating & copying/moving. Cannot be relied upon to always set m_count or m_capacity.
		 *	@details Allocation will be done prior to copy_or_move. Hence, if copy_or_move is noexcept it can be destructive (move) from other's values without concern for revertibility.
		 *	@param other The source of values to copy or move.
		 *	@param copy_or_move The adapter function to cast the value to the type from which to construct.
		 *	@tparam Buckets Either buckets or const buckets.
		 *	@tparam CopyOrMove Accepts the copied value and can return it either by reference or as a move-reference.
		 */
		template <typename BucketsType, typename CopyOrMove>
		void allocate_and_initialize_one_or_more_values(BucketsType& other, CopyOrMove&& copy_or_move)
		{
			SH_ROBINHOOD_ASSERT(other.m_capacity > 0);
			// Since other.m_capacity is non-zero, m_info & m_data must be allocated;
			m_data = rebind_traits<value_type>::allocate(get_allocator(), other.m_capacity);
			other.with_info([this, &other, &copy_or_move](const auto* const other_info)
			{
				using other_info_type = std::decay_t<decltype(*other_info)>;
				auto info_alloc = get_info_allocator<other_info_type>();
				// Allocate & store this bucket's new info.
				auto* const info = rebind_traits<other_info_type>::allocate(info_alloc, other.m_capacity + info_tail);
				m_info = info;
				if (other.m_count == 0)
				{
					// other is empty, just clear our newly allocated info.
					clear_info(info, other.m_capacity + info_tail);
				}
				else
				{
					// other is non-empty
					initialize_values(other, other_info, std::forward<CopyOrMove>(copy_or_move));
				}
			});
			// Does not always set m_count or m_capacity!
		}
		/**	Allocates bucket space and initializes by copy one or more possible values from given buckets.
		 *	@param other The source of values to copy.
		 */
		void allocate_and_initialize_one_or_more_values_by_copy(const buckets& other)
		{
			allocate_and_initialize_one_or_more_values(other, [](const value_type& value) -> const value_type& { return value; });
		}
		/**	Allocates bucket space and initializes by move_if_noexcept one or more possible values from given buckets.
		 *	@param other The source of values to move.
		 */
		void allocate_and_initialize_one_or_more_values_by_move(buckets& other)
		{
			allocate_and_initialize_one_or_more_values(other, [](value_type& value) noexcept { return std::move_if_noexcept(value); });
		}
		/**	Allocates bucket space and initializes values from another buckets.
		 *	@note Does not destroy or check existing data or info prior to allocating & copying/moving. Cannot be relied upon to always set m_count or m_capacity.
		 *	@param other The source of values to copy or move.
		 *	@param copy_or_move The adapter function to cast the value to the type from which to construct.
		 *	@tparam Buckets Either buckets or const buckets.
		 *	@tparam CopyOrMove Accepts the copied value and can return it either by reference or as a move-reference.
		 */
		template <typename BucketsType, typename CopyOrMove>
		void allocate_and_initialize_values(BucketsType& other, CopyOrMove&& copy_or_move)
		{
			static_assert(std::is_same_v<std::decay_t<BucketsType>, buckets>);
			// Does not destruct m_info or m_data!
			if (other.m_capacity == 0)
			{
				// When m_capacity is zero, m_info should be empty_info<info_type> and m_data, nullptr. We can avoid
				// testing to find info_type here and just copy other.m_info as it's known to reference the correct
				// info_type.
				m_info = other.m_info;
				m_data = nullptr;
			}
			else
			{
				allocate_and_initialize_one_or_more_values(other, std::forward<CopyOrMove>(copy_or_move));
			}
			// Does not always set m_count or m_capacity!
		}
		/**	Allocates bucket space initializes values by copy from given buckets.
		 *	@param other The source of values to copy.
		 */
		void allocate_and_initialize_values_by_copy(const buckets& other)
		{
			allocate_and_initialize_values(other, [](const value_type& value) -> const value_type& { return value; });
		}
		/**	Allocates bucket space and initializes values by move_if_noexcept from given buckets.
		 *	@param other The source of values to move.
		 */
		void allocate_and_initialize_values_by_move(buckets& other)
		{
			allocate_and_initialize_values(other, [](value_type& value) noexcept { return std::move_if_noexcept(value); });
		}

		/**	Replaces values from given buckets with the same capacity & get_wide values as ours.
		 *	@note Appropriately handles if this has allocated values.
		 *	@param other The source of values to copy or move.
		 *	@param copy_or_move The adapter function to cast the value to the type from which to construct.
		 *	@tparam Buckets Either buckets or const buckets.
		 *	@tparam CopyOrMove Accepts the copied value and can return it either by reference or as a move-reference.
		 */
		template <typename BucketsType, typename CopyOrMove>
		void replace_values_from_same_sized_buckets(BucketsType& other, CopyOrMove&& copy_or_move)
		{
			static_assert(std::is_same_v<std::decay_t<BucketsType>, buckets>, "Only expects to be given a buckets type.");
			// Assert preconditions:
			SH_ROBINHOOD_ASSERT(m_capacity == other.m_capacity);
			SH_ROBINHOOD_ASSERT(get_wide() == other.get_wide());
			if (m_count == 0)
			{
				// A m_count of zero means there's nothing constructed to destroy in this.
				if (other.m_count > 0)
				{
					// m_other's count indicates that it does have one or more values to copy.
					with_info([this, &other, &copy_or_move](auto* const info)
					{
						using info_type = std::decay_t<decltype(*info)>;
						// We know other_info's type to be the same type as our info_type as m_capacity & get_wide() were matched above.
						const auto* const other_info = other.template get_info<info_type>();
						if constexpr (is_trivial_v<value_type>)
						{
							// Do a trivial copy of info & data from other to this.
							copy_trivial_info(info, other_info, m_capacity); // no + info_tail, it's already set
							copy_trivial_values(m_data, other.m_data, other.m_capacity);
						}
						else
						{
							// Copy info & data from other where non-empty to this. We know this has no currently constructed values.
							for (size_type index = 0; index < other.m_capacity; ++index)
							{
								// As our m_count is zero, there are no values to destroy_value.
								if (other_info[index].empty() == false)
								{
									construct_value(index, copy_or_move(other.get_value(index)));
								}
								// Outside of the conditional to allow additional optimization.
								info[index] = other_info[index];
							}
						}
					});
				}
				/*
				else
				{
					// both m_count == 0: there's nothing to do as both info and values are empty in both buckets.
				}
				*/
			}
			else if (other.m_count == 0)
			{
				// Our m_count is non-zero, but other has no constructed values. Clear our info and destroy out values.
				with_info([this, &other](auto* const info) noexcept
				{
					if constexpr (is_trivial_v<value_type>)
					{
						// Do a trivial clear of info & leave data untouched as it's trivial.
						clear_info(info, m_capacity); // no + info_tail
					}
					else
					{
						// Clear info & data where non-empty. We know other has no currently constructed values.
						for (size_type index = 0; index < other.m_capacity; ++index)
						{
							if (info[index].empty() == false)
							{
								destroy_value(index);
							}
							// Outside of the conditional to allow additional optimization.
							info[index].clear();
						}
					}
				});
			}
			else
			{
				// Both buckets' m_count is non-zero and contain constructed values.
				with_info([this, &other, &copy_or_move](auto* const info)
				{
					using info_type = std::decay_t<decltype(*info)>;
					// We know other_info's type to be the same type as our info_type as m_capacity & get_wide() were matched above.
					const info_type* const other_info = other.template get_info<info_type>();
					if constexpr (is_trivial_v<value_type>)
					{
						// Do a trivial copy of info & data from other to this.
						copy_trivial_info(info, other_info, m_capacity); // no + info_tail, it's already set
						copy_trivial_values(m_data, other.m_data, other.m_capacity);
					}
					else
					{
						// Copy info & data from other where non-empty to this. Our data may already be constructed.
						for (size_type index = 0; index < other.m_capacity; ++index)
						{
							if (info[index].empty() == false)
							{
								destroy_value(index);
#ifndef NDEBUG
								// Info is non-empty, clear it for the asserts in construct_value.
								info[index].clear();
#endif // !NDEBUG
							}
							if (other_info[index].empty() == false)
							{
								construct_value(index, copy_or_move(other.get_value(index)));
							}
							// Outside of the conditional to allow additional optimization.
							info[index] = other_info[index];
						}
					}
				});
			}
		}
		/**	Copies values from another buckets that's the same capacity & get_wide type as ourself.
		 *	@param other The source of values to copy.
		 */
		void copy_replace_values_from_same_sized_buckets(const buckets& other)
		{
			replace_values_from_same_sized_buckets(other, [](const value_type& value) -> const value_type& { return value; });
		}
		/**	Moves values from another buckets that's the same capacity & get_wide type as ourself.
		 *	@param other The source of values to move.
		 */
		void move_replace_values_from_same_sized_buckets(buckets& other) noexcept
		{
			replace_values_from_same_sized_buckets(other, [](value_type& value) noexcept { return std::move_if_noexcept(value); });
		}

		/**	Destruct all constructed values without updating info. Does not update count.
		 *	@param info A pointer to the current (m_capacity + info_tail) element array of Robinhood info elements.
		 *	@tparam InfoType The type of Robinhood info elements in the info element array.
		 */
		template <typename InfoType>
		void destroy_values(InfoType* const info) noexcept
		{
			for (size_type index = 0; index < m_capacity; ++index)
			{
				if (info[index].empty() == false)
				{
					destroy_value(index);
				}
			}
		}
		/**	Destruct all constructed values and clear info. Does not update count.
		 *	@param info A pointer to the current (m_capacity + info_tail) element array of Robinhood info elements.
		 *	@tparam InfoType The type of Robinhood info elements in the info element array.
		 */
		template <typename InfoType>
		void destruct_values_and_clear_info(InfoType* const info) noexcept
		{
			for (size_type index = 0; index < m_capacity; ++index)
			{
				if (info[index].empty() == false)
				{
					destroy_value(index);
				}
				// Outside of the conditional to allow additional optimization.
				info[index].clear();
			}
		}
		/**	Destruct all constructed values and deallocate the memory. Does not update count or reset/nullify info or data.
		 *	@note If m_count is zero, will skip iterating the info & destructing values.
		 */
		void destruct_and_deallocate_values_and_info() noexcept
		{
			// If m_capacity > 0, deallocation of info & values is required.
			if (m_capacity > 0)
			{
				with_info([this](auto* const info) noexcept
				{
					using info_type = std::decay_t<decltype(*info)>;
					// If value_type is trivially destructible, skip the iteration over info done in destroy_values.
					if constexpr (std::is_trivially_destructible_v<value_type> == false)
					{
						// If m_count == 0, there are no values to destruct.
						if (m_count > 0)
						{
							destroy_values(info);
						}
					}
					auto info_alloc = get_info_allocator<info_type>();
					rebind_traits<info_type>::deallocate(info_alloc, info, m_capacity + info_tail);
				});
				rebind_traits<value_type>::deallocate(get_allocator(), m_data, m_capacity);
			}
		}

#ifndef NDEBUG
		/**	Return true if the given index is empty (hence, destructed).
		 *	@note Only asserts that index within bounds and currently destructed.
		 *	@param info A pointer to the current (m_capacity + info_tail) element array of Robinhood info elements.
		 *	@param index The index from [0, m_capacity + info_tail) to test for emptiness.
		 *	@return True if the given index is empty.
		 *	@tparam InfoType The type of Robinhood info elements in the info element array.
		 */
		template <typename InfoType>
		bool empty(const InfoType* const info, const size_type index) const noexcept
		{
			SH_ROBINHOOD_ASSERT(index <= m_capacity);
			SH_ROBINHOOD_ASSERT(index < m_capacity || info[index].get_distance() == empty_distance_v<typename InfoType::distance_type>);
			return info[index].empty();
		}

		/**	Return true if the given index is empty (hence, destructed).
		 *	@note Only asserts that index within bounds and currently destructed.
		 *	@param index The index from [0, m_capacity + info_tail) to test for emptiness.
		 *	@return True if the given index is empty.
		 */
		bool empty(const size_type index) const noexcept
		{
			return with_info([this, index](const auto* const info) noexcept -> bool
			{
				return empty(info, index);
			});
		}
#endif // !NDEBUG

		/**	Possibly pointing to narrow_info_type or wide_info_type, depending on the value of get_wide().
		 *	@details Never null. Has m_capacity + info_tail elements.
		 *	@note When m_capacity is zero, this should be equal to &empty_info().
		 */
		info_pointer m_info;
		/**	Contains a pointer to an array of storage of (possibly unconstructed) objects of value_type.
		 *	@details Expected to be null if m_capacity is zero. Otherwise, has m_capacity elements
		 */
		pointer m_data;
		/**	The storage capacity.
		 */
		size_type m_capacity;
		/**	The number of currently constructed objects of value_type held in m_data.
		 */
		size_type m_count;
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
		/**	The current "generation" of the data to be used for debug iterators.
		 *	@details Each iterator-invalidating modification of the table buckets will update this value, causing captured iterator generations to fail to match.
		 */
		size_type m_generation;
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
	};

	/**	An iterator base class for an open-addressing Robinhood hashtable with variable info width.
	 *	@tparam BucketsType The type of the Robinhood buckets.
	 */
	template <typename BucketsType>
	class iterator_base
	{
	protected:
		using buckets_type = BucketsType;

	public:
		using iterator_category = std::bidirectional_iterator_tag;
		using size_type = typename buckets_type::size_type;
		using difference_type = std::ptrdiff_t;

		/**	A default-initializing constructor.
		 */
		constexpr iterator_base() noexcept
			: m_buckets(nullptr)
			, m_index(0)
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			, m_debug{}
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
		{ }
		~iterator_base() = default;
		iterator_base(const iterator_base& other) = default;
		iterator_base(iterator_base&& other) noexcept = default;
		template <typename OtherBucketsType>
		iterator_base(const iterator_base<OtherBucketsType>& other) noexcept
			: m_buckets(other.get_buckets())
			, m_index(other.get_index())
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			, m_debug(other.get_debug())
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
		{ }
		iterator_base& operator=(const iterator_base& other) = default;
		iterator_base& operator=(iterator_base&& other) noexcept = default;
		template <typename OtherBucketsType>
		constexpr iterator_base& operator=(const iterator_base<OtherBucketsType>& other) noexcept
		{
			m_buckets = other.get_buckets();
			m_index = other.get_index();
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			m_debug = other.get_debug();
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
			return *this;
		}

		constexpr void decrement() noexcept
		{
			// Decrementing a default iterator is undefined behavior. Also, required for following asserts.
			SH_ROBINHOOD_ASSERT(get_buckets() != nullptr);
			// If index is zero, we're going to decrement past the begin iterator.
			SH_ROBINHOOD_ASSERT(get_index() > 0);
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			SH_ROBINHOOD_ASSERT(get_buckets()->check_iterator_debug(get_index(), get_debug()));
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
			get_buckets()->with_info([this](const auto* const info) noexcept
			{
				do
				{
					--m_index;
				} while(m_index != 0 && info[m_index].empty());
			});
		}
		constexpr void increment() noexcept
		{
			// Incrementing a default iterator is undefined behavior. Also, required for following asserts.
			SH_ROBINHOOD_ASSERT(get_buckets() != nullptr);
			// If index is capacity, we're going to increment past the end iterator.
			SH_ROBINHOOD_ASSERT(get_index() < get_buckets()->get_capacity());
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			SH_ROBINHOOD_ASSERT(get_buckets()->check_iterator_debug(get_index(), get_debug()));
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
			get_buckets()->with_info([this](const auto* const info) noexcept
			{
				const size_type capacity = get_buckets()->get_capacity();
				do
				{
					++m_index;
				} while(m_index != capacity && info[m_index].empty());
			});
		}

		template <typename OtherBucketsType>
		constexpr bool operator==(const iterator_base<OtherBucketsType>& it) const noexcept
		{
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			// Default constructed iterators must be allowed to compare and return equal.
			SH_ROBINHOOD_ASSERT(get_buckets() == nullptr || get_buckets()->check_iterator_debug(get_index(), get_debug()));
			SH_ROBINHOOD_ASSERT(it.get_buckets() == nullptr || it.get_buckets()->check_iterator_debug(it.get_index(), it.get_debug()));
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
			return get_index() == it.get_index();
		}
		template <typename OtherBucketsType>
		constexpr bool operator!=(const iterator_base<OtherBucketsType>& it) const noexcept
		{
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			// Default constructed iterators must be allowed to compare and return equal (false, in this case).
			SH_ROBINHOOD_ASSERT(get_buckets() == nullptr || get_buckets()->check_iterator_debug(get_index(), get_debug()));
			SH_ROBINHOOD_ASSERT(it.get_buckets() == nullptr || it.get_buckets()->check_iterator_debug(it.get_index(), it.get_debug()));
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
			return get_index() != it.get_index();
		}

	protected:
		template <typename OtherBucketsType>
		friend class iterator_base;

		constexpr iterator_base(buckets_type* const buckets, const size_type index) noexcept
			: m_buckets(buckets)
			, m_index(index)
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			, m_debug(buckets != nullptr ? buckets->get_iterator_debug() : iterator_debug<size_type>{})
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
		{ }

		/**	Return a reference to the value in buckets to which this iterator refers.
		 *	@note Will return a constant reference if buckets_type is const. Will be a mutable reference otherwise.
		 *	@return A reference to the value in buckets.
		 */
		constexpr auto& get_value() const noexcept
		{
			// Incrementing a default iterator is undefined behavior. Also, required for following asserts.
			SH_ROBINHOOD_ASSERT(get_buckets() != nullptr);
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			SH_ROBINHOOD_ASSERT(get_buckets()->check_iterator_debug(get_index(), get_debug()));
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
			// buckets_type::get_value will assert that index is within [0, capacity)
			return get_buckets()->get_value(get_index());
		}
		constexpr buckets_type* get_buckets() const noexcept
		{
			return m_buckets;
		}
		constexpr size_type get_index() const noexcept
		{
			return m_index;
		}
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
		constexpr const iterator_debug<size_type>& get_debug() const noexcept
		{
			return m_debug;
		}
#endif // SH_ROBINHOOD_DEBUG_ITERATOR

		/**	A pointer to the buckets container.
		 *	@note Expected to only be nullptr if default initialized.
		 */
		buckets_type* m_buckets;

		/**	A index of the item in the buckets container to which this iterator refers.
		 *	@note If m_buckets is null, this should be zero.
		 */
		size_type m_index;

#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
		/**	The iterator_debug information from construction or assignment. Used to verify if this iterator is canonically valid when used.
		 *	@note If m_buckets is null, this should be zero-initialized.
		 */
		iterator_debug<size_type> m_debug;
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
	};

	template <typename HashTable, typename BucketsType>
	class const_iterator;

	/**	An iterator for an open-addressing Robinhood hashtable with variable info width.
	 *	@tparam HashTable The type of the hashtable.
	 *	@tparam BucketsType The type of the Robinhood buckets.
	 */
	template <typename HashTable, typename BucketsType>
	class iterator : private iterator_base<BucketsType>
	{
	private:
		using hashtable_type = HashTable;
		using base_type = iterator_base<BucketsType>;
		using typename base_type::buckets_type;

	public:
		using value_type = typename hashtable_type::exposed_value_type;
		using reference = value_type&;
		using pointer = value_type*;
		using typename base_type::iterator_category;
		using typename base_type::size_type;
		using typename base_type::difference_type;

		iterator() = default;
		~iterator() = default;
		iterator(const iterator& other) = default;
		iterator(iterator&& other) noexcept = default;
		iterator& operator=(const iterator& other) = default;
		iterator& operator=(iterator&& other) noexcept = default;

		constexpr reference operator*() const noexcept
		{
			return static_cast<reference>(this->base_type::get_value());
		}
		constexpr pointer operator->() const noexcept
		{
			return std::addressof(this->operator*());
		}

		constexpr iterator& operator--() noexcept
		{
			this->base_type::decrement();
			return *this;
		}
		constexpr iterator operator--(int) noexcept
		{
			const iterator result = *this;
			this->base_type::decrement();
			return result;
		}
		constexpr iterator& operator++() noexcept
		{
			this->base_type::increment();
			return *this;
		}
		constexpr iterator operator++(int) noexcept
		{
			const iterator result = *this;
			this->base_type::increment();
			return result;
		}
		constexpr friend bool operator==(const iterator& lhs, const iterator& rhs) noexcept
		{
			return lhs.base_type::operator==(rhs);
		}
		constexpr friend bool operator!=(const iterator& lhs, const iterator& rhs) noexcept
		{
			return lhs.base_type::operator!=(rhs);
		}

	private:
		friend hashtable_type;
		friend class const_iterator<hashtable_type, buckets_type>;

		constexpr iterator(buckets_type& buckets, const size_type index) noexcept
			: base_type(&buckets, index)
		{ }
	};

	/**	A const iterator for an open-addressing Robinhood hashtable with variable info width.
	 *	@tparam HashTable The type of the hashtable.
	 *	@tparam BucketsType The type of the Robinhood buckets.
	 */
	template <typename HashTable, typename BucketsType>
	class const_iterator : private iterator_base<const BucketsType>
	{
	private:
		using hashtable_type = HashTable;
		using buckets_type = BucketsType;
		using base_type = iterator_base<const BucketsType>;
		using const_buckets_type = typename base_type::buckets_type;

	public:
		using value_type = const typename hashtable_type::exposed_value_type;
		using reference = value_type&;
		using pointer = value_type*;
		using typename base_type::iterator_category;
		using typename base_type::size_type;
		using typename base_type::difference_type;

		const_iterator() = default;
		~const_iterator() = default;
		const_iterator(const const_iterator& other) = default;
		const_iterator(const_iterator&& other) noexcept = default;
		constexpr const_iterator(const iterator<hashtable_type, buckets_type>& other) noexcept
			: base_type(other)
		{ }
		const_iterator& operator=(const const_iterator& other) = default;
		const_iterator& operator=(const_iterator&& other) noexcept = default;
		constexpr const_iterator& operator=(const iterator<hashtable_type, buckets_type>& other) noexcept
		{
			this->base_type::operator=(other);
			return *this;
		}

		constexpr reference operator*() const noexcept
		{
			return static_cast<reference>(this->base_type::get_value());
		}
		constexpr pointer operator->() const noexcept
		{
			return std::addressof(this->operator*());
		}

		constexpr const_iterator& operator--() noexcept
		{
			this->base_type::decrement();
			return *this;
		}
		constexpr const_iterator operator--(int) noexcept
		{
			const const_iterator result = *this;
			this->base_type::decrement();
			return result;
		}
		constexpr const_iterator& operator++() noexcept
		{
			this->base_type::increment();
			return *this;
		}
		constexpr const_iterator operator++(int) noexcept
		{
			const const_iterator result = *this;
			this->base_type::increment();
			return result;
		}

		constexpr friend bool operator==(const const_iterator& lhs, const const_iterator& rhs) noexcept
		{
			return lhs.base_type::operator==(rhs);
		}
		constexpr bool operator==(const iterator<hashtable_type, buckets_type>& it) noexcept
		{
			return this->base_type::operator==(it);
		}
		constexpr friend bool operator==(const iterator<hashtable_type, buckets_type>& lhs, const const_iterator& rhs) noexcept
		{
			return rhs == lhs;
		}
		constexpr friend bool operator!=(const const_iterator& lhs, const const_iterator& rhs) noexcept
		{
			return lhs.base_type::operator!=(rhs);
		}
		constexpr bool operator!=(const iterator<hashtable_type, buckets_type>& it) noexcept
		{
			return this->base_type::operator!=(it);
		}
		constexpr friend bool operator!=(const iterator<hashtable_type, buckets_type>& lhs, const const_iterator& rhs) noexcept
		{
			return rhs != lhs;
		}

	private:
		friend hashtable_type;

		constexpr const_iterator(const_buckets_type& buckets, const size_type index) noexcept
			: base_type(&buckets, index)
		{ }
	};

	/**	Wraps Robinhood buckets in a easily adaptable facade for std::unordered_set and std::unordered_map types of application.
	 *	@tparam PolicyType A specialization of robinhood::policy.
	 *	@tparam Hash A (specialized) std::hash-type functor.
	 *	@tparam KeyEqual A (specialized) std::equal_to functor.
	 *	@tparam Allocates A (specialized) std::allocator functor.
	 */
	template <typename PolicyType, typename Hash, typename KeyEqual, typename Allocator>
	class hashtable : public KeyEqual, public Hash
	{
	public:
		using hashtable_type = hashtable<PolicyType, Hash, KeyEqual, Allocator>;
		using policy_type = PolicyType;
		using hasher = Hash;
		using key_equal = KeyEqual;
		using allocator_type = Allocator;
		using allocator_traits = std::allocator_traits<allocator_type>;
		using key_type = typename policy_type::key_type;
		using size_type = typename policy_type::size_type;
		using exposed_value_type = typename policy_type::exposed_value_type;
		using mutable_value_type = typename policy_type::mutable_value_type;
		using wide_distance_type = typename policy_type::wide_distance_type;
		using narrow_info_type = typename policy_type::narrow_info_type;
		using wide_info_type = typename policy_type::wide_info_type;
		using buckets_type = buckets<policy_type, allocator_type>;
		using local_iterator = typename buckets_type::iterator;
		using const_local_iterator = typename buckets_type::const_iterator;
		using iterator = robinhood::iterator<hashtable_type, buckets_type>;
		using const_iterator = robinhood::const_iterator<hashtable_type, buckets_type>;
		using hash_result = std::size_t;
		using bits_type = std::uint32_t;

		constexpr static bool is_transparent = is_transparent_v<hasher> && is_transparent_v<key_equal>;

		// Required in order to use the more efficient path of do_rehash_larger.
		// Required for do_rehash_smaller, do_emplace, do_widen_and_continue_emplace, do_erase (for buckets::move_back).
		static_assert(std::is_nothrow_move_constructible_v<mutable_value_type>, "mutable_value_type expected to have noexcept move constructor.");
		// Required for do_rehash_smaller, do_emplace, do_widen_and_continue_emplace.
		static_assert(std::is_nothrow_swappable_v<mutable_value_type>, "mutable_value_type expected to have noexcept swap operation.");

		/**	The result of do_emplace.
		 */
		struct emplace_result final
		{
			/**	The bucket index where the emplaced value can be found.
			 */
			size_type m_index;
		};

		/**	The result of do_find.
		 */
		struct find_result final
		{
			/**	The bucket index where find completed.
			 */
			size_type m_index;

			/** The Robinhood distance at search completion.
			 */
			wide_distance_type m_distance;

			/** If true, the bucket index where find matched a value. If false, where the search completed.
			 */
			bool m_found;
		};

		constexpr static float default_load_factor = 0.875f;
		static_assert(default_load_factor > 0.0f, "default_load_factor must be greater than 0.");
		static_assert(default_load_factor <= 1.0f, "default_load_factor cannot be greater than 1.");

		explicit constexpr hashtable(const allocator_type& alloc)
			noexcept(std::is_nothrow_constructible_v<key_equal>
				&& std::is_nothrow_constructible_v<hasher>
				&& std::is_nothrow_constructible_v<buckets_type, const allocator_type&>)
			: key_equal()
			, hasher()
			, m_buckets(typename buckets_type::allocator_type(alloc))
			, m_max_load_factor(default_load_factor)
			, m_shift_bits(shift_bits<hash_result>(m_buckets.get_capacity()))
		{ }
		hashtable(const size_type bucket_count, const hasher& hash, const key_equal& equal, const allocator_type& alloc)
			: key_equal(equal)
			, hasher(hash)
			, m_buckets(round_up_bucket_count(bucket_count), typename buckets_type::allocator_type(alloc))
			, m_max_load_factor(default_load_factor)
			, m_shift_bits(shift_bits<hash_result>(m_buckets.get_capacity()))
		{ }
		hashtable(const size_type bucket_count, const hasher& hash, const key_equal& equal)
			: hashtable(bucket_count, hash, equal, allocator_type())
		{ }
		hashtable(const hashtable& other, const allocator_type& alloc)
			: key_equal(other)
			, hasher(other)
			, m_buckets(other.m_buckets, typename buckets_type::allocator_type(alloc))
			, m_max_load_factor(other.m_max_load_factor)
			, m_shift_bits(other.m_shift_bits)
		{ }
		hashtable(const hashtable& other)
			: key_equal(other)
			, hasher(other)
			, m_buckets(other.m_buckets)
			, m_max_load_factor(other.m_max_load_factor)
			, m_shift_bits(other.m_shift_bits)
		{ }
		hashtable(hashtable&& other, const allocator_type& alloc)
			noexcept(std::is_nothrow_move_constructible_v<key_equal>
				&& std::is_nothrow_move_constructible_v<hasher>
				&& std::is_nothrow_constructible_v<typename buckets_type::allocator_type, const allocator_type&>
				&& std::is_nothrow_constructible_v<buckets_type, buckets_type&&, typename buckets_type::allocator_type&&>)
			: key_equal(std::move(other))
			, hasher(std::move(other))
			, m_buckets(std::move(other.m_buckets), typename buckets_type::allocator_type(alloc))
			, m_max_load_factor(other.m_max_load_factor)
			, m_shift_bits(std::exchange(other.m_shift_bits, shift_bits<hash_result>(other.m_buckets.get_capacity())))
		{ }
		hashtable(hashtable&& other)
			noexcept(std::is_nothrow_move_constructible_v<key_equal>
				&& std::is_nothrow_move_constructible_v<hasher>
				&& std::is_nothrow_move_constructible_v<buckets_type>)
			: key_equal(std::move(other))
			, hasher(std::move(other))
			, m_buckets(std::move(other.m_buckets))
			, m_max_load_factor(other.m_max_load_factor)
			, m_shift_bits(std::exchange(other.m_shift_bits, shift_bits<hash_result>(other.m_buckets.get_capacity())))
		{ }
		~hashtable() = default;

		hashtable& operator=(const hashtable& other)
		{
			this->key_equal::operator=(other);
			this->hasher::operator=(other);
			m_buckets = other.m_buckets;
			m_max_load_factor = other.m_max_load_factor;
			m_shift_bits = other.m_shift_bits;
			return *this;
		}
		hashtable& operator=(hashtable&& other)
			noexcept(std::is_nothrow_move_assignable_v<buckets_type>)
		{
			// Move buckets first. That way, if it throws, the other values are left intact.
			m_buckets = std::move(other.m_buckets);
			static_assert(std::is_nothrow_move_assignable_v<key_equal>, "key_equal must have noexcept move assignment operation.");
			this->key_equal::operator=(std::move(other));
			static_assert(std::is_nothrow_move_assignable_v<hasher>, "hasher must have noexcept move assignment operation.");
			this->hasher::operator=(std::move(other));
			m_max_load_factor = other.m_max_load_factor;
			m_shift_bits = std::exchange(other.m_shift_bits, shift_bits<hash_result>(other.m_buckets.get_capacity()));
			return *this;
		}
		allocator_type get_allocator() const
			noexcept(std::is_nothrow_constructible_v<allocator_type, const typename buckets_type::allocator_type&>)
		{
			return allocator_type(m_buckets.get_allocator());
		}

		template <typename KeyArg>
		constexpr size_type bucket(KeyArg&& key) const
			noexcept(noexcept(std::declval<const hasher&>()(std::forward<KeyArg>(key))))
		{
			return hash_clamp_with_shift_bits(static_cast<const hasher&>(*this)(std::forward<KeyArg>(key)));
		}
		constexpr bool empty() const noexcept
		{
			return m_buckets.get_count() == 0;
		}
		constexpr size_type size() const noexcept
		{
			return m_buckets.get_count();
		}
		constexpr size_type max_size() const noexcept
		{
			// distance of zero is used to indicate empty
			return std::numeric_limits<size_type>::max() - 1;
		}
		constexpr size_type bucket_count() const noexcept
		{
			return m_buckets.get_capacity();
		}
		constexpr size_type max_bucket_count() const noexcept
		{
			return max_size();
		}
		constexpr size_type bucket_size(const size_type n) const noexcept
		{
			SH_ROBINHOOD_ASSERT(n < m_buckets.get_capacity());
			return m_buckets.with_info([n](const auto* const info) noexcept -> size_type
			{
				return info[n].empty() ? 0 : 1;
			});
		}
		constexpr float load_factor() const noexcept
		{
			return load_factor_with_given_size_current_capacity(size());
		}
		constexpr void max_load_factor(const float ml) noexcept
		{
			m_max_load_factor = std::min(1.0f, ml);
		}
		constexpr float max_load_factor() const noexcept
		{
			return m_max_load_factor;
		}
		constexpr hasher hash_function() const
			noexcept(std::is_nothrow_copy_constructible_v<hasher>)
		{
			return static_cast<const hasher&>(*this);
		}
		constexpr key_equal key_eq() const
			noexcept(std::is_nothrow_copy_constructible_v<key_equal>)
		{
			return static_cast<const key_equal&>(*this);
		}

		constexpr size_type get_index(const const_iterator pos) const noexcept
		{
			SH_ROBINHOOD_ASSERT(&m_buckets == pos.get_buckets());
			return pos.get_index();
		}
		constexpr iterator index(const size_type n) noexcept
		{
			return iterator(m_buckets, n);
		}
		constexpr const_iterator index(const size_type n) const noexcept
		{
			return const_iterator(m_buckets, n);
		}
		constexpr iterator begin() noexcept
		{
			return index(m_buckets.with_info([this](const auto* const info) noexcept -> size_type
			{
				size_type index = 0;
				while (index != m_buckets.get_capacity() && info[index].empty())
				{
					++index;
				}
				return index;
			}));
		}
		constexpr iterator end() noexcept
		{
			return index(m_buckets.get_capacity());
		}
		constexpr const_iterator begin() const noexcept
		{
			return index(m_buckets.with_info([this](const auto* const info) noexcept -> size_type
			{
				size_type index = 0;
				while (index != m_buckets.get_capacity() && info[index].empty())
				{
					++index;
				}
				return index;
			}));
		}
		constexpr const_iterator end() const noexcept
		{
			return index(m_buckets.get_capacity());
		}
		constexpr local_iterator begin(const size_type n) noexcept
		{
			return m_buckets.get_value_address(n);
		}
		constexpr local_iterator end(const size_type n) noexcept
		{
			return m_buckets.get_value_address(n + bucket_size(n));
		}
		constexpr const_local_iterator begin(const size_type n) const noexcept
		{
			return m_buckets.get_value_address(n);
		}
		constexpr const_local_iterator end(const size_type n) const noexcept
		{
			return m_buckets.get_value_address(n + bucket_size(n));
		}

		void clear() noexcept
		{
			m_buckets.clear();
		}

		void reserve(size_type new_bucket_count)
		{
			new_bucket_count = round_up_bucket_count(ceilui<size_type>(new_bucket_count / m_max_load_factor));
			if (new_bucket_count > bucket_count())
			{
				do_rehash_larger(new_bucket_count);
			}
		}
		void rehash(size_type new_bucket_count)
		{
			const size_type min_bucket_count = size() / max_load_factor();
			new_bucket_count = round_up_bucket_count(std::max(new_bucket_count, min_bucket_count));
			if (new_bucket_count != bucket_count())
			{
				do_rehash(new_bucket_count);
			}
		}
		/**	Swap operation with a given hashtable.
		 *	@param other The hashtable with which to swap contents.
		 */
		void swap(hashtable_type& other)
			noexcept(std::is_nothrow_swappable_v<buckets_type>)
		{
			using std::swap;
			// Swap m_buckets first. That way, if it throws, the other values are left intact.
			m_buckets.swap(other.m_buckets);
			static_assert(std::is_nothrow_swappable_v<key_equal>, "key_equal must have noexcept swap operation.");
			swap(static_cast<key_equal&>(*this), static_cast<key_equal&>(other));
			static_assert(std::is_nothrow_swappable_v<hasher>, "hasher must have noexcept swap operation.");
			swap(static_cast<hasher&>(*this), static_cast<hasher&>(other));
			swap(m_max_load_factor, other.m_max_load_factor);
			swap(m_shift_bits, other.m_shift_bits);
		}
		/**	Swap two hashtable's contents.
		 *	@param lhs A hashtable to swap contents with rhs.
		 *	@param rhs A hashtable to swap contents with lhs.
		 */
		friend void swap(hashtable_type& lhs, hashtable_type& rhs)
			noexcept(noexcept(lhs.swap(rhs)))
		{
			return lhs.swap(rhs);
		}

	private:
		/**	Called by do_emplace variants to check if the maximum Robinhood distance has been reached, either to widen or throw.
		 *	@tparam InfoType The type of Robinhood info being used by buckets.
		 *	@return The maximum Robinhood distance for the given InfoType.
		 */
		template <typename InfoType>
		constexpr static typename InfoType::distance_type max_info_distance() noexcept
		{
			if constexpr (policy_type::constant_width)
			{
				// If constant_width, narrow_info_type == wide_info_type. We
				// have to leave room for plus one past all entries as do_find
				// may take advantage of that distance value for an empty bucket.
				return narrow_info_type::max_distance() - 1;
			}
			else if constexpr (std::is_same_v<InfoType, narrow_info_type>)
			{
				// Since not constant_width, wide_info_type has more room than
				// narrow_info_type and we can get away with using the max_distance.
				return narrow_info_type::max_distance();
			}
			else
			{
				static_assert(std::is_same_v<InfoType, wide_info_type>);
				// Much like the constant_width case above, we need to leave room
				// for plus one past all entries in do_find. The prevents do_find
				// from having to check for numeric overflow, since it cannot occur.
				return wide_info_type::max_distance() - 1;
			}
		}

		/**	Rehash the buckets to a given bucket count that is greater than or equal to the current bucket count.
		 *	@note Does not check if the current and desired bucket count are equal before performing the rehash.
		 *	@param new_bucket_count The desired bucket count.
		 */
		void do_rehash_larger(const size_type new_bucket_count)
		{
			SH_ROBINHOOD_ASSERT(new_bucket_count >= m_buckets.get_capacity());

			// If wide, pessimisticly assume that whatever horrible anomaly resulted in going wide before will remain or
			// could happen again post-rehash.
			buckets_type new_buckets(new_bucket_count, m_buckets.get_wide(), m_buckets.get_allocator());
			const bits_type new_shift_bits = shift_bits<hash_result>(new_bucket_count);

			// If it's safe, use move operations for a "fast path".
			constexpr bool use_move = std::is_nothrow_move_constructible_v<mutable_value_type>
				&& std::is_nothrow_move_assignable_v<buckets_type>;

			// If rehashing to a larger size, narrow info should never need to widen. This is only true as long as power-
			// of-two sizes & a shifting type of hash_clamp are used.
			m_buckets.with_info([this, &new_buckets, new_shift_bits](const auto* const info)
			{
				using info_type = std::decay_t<decltype(*info)>;
				auto* const new_info = new_buckets.template get_info<info_type>();
				const size_type capacity = m_buckets.get_capacity();
				for (size_type index = 0; index < capacity; ++index)
				{
					if (info[index].empty() == false)
					{
						mutable_value_type& value = m_buckets.get_value(index);
						// hasher cannot safely throw here, but not all are marked noexcept!
						const hash_result hash = static_cast<const hasher&>(*this)(value.key());
						const size_type dst = hash_clamp(hash, new_shift_bits);
						if constexpr (use_move)
						{
							// The fast path here, if move construction and bucket move assignment are noexcept:
							do_emplace(new_buckets, new_info, std::move(value), info_type(1, hash), dst);
							// Destruct value now, so that we don't have to reiterate over m_buckets when it's move
							// assigned below.
							m_buckets.destroy(value);
						}
						else
						{
							// If there's a possibility of mutable_value_type move construction throwing an exception, pass by
							// copy. This way the previous location (value) is still intact in m_buckets if an exception is
							// thrown and we avoid the rewind (which could throw again!).
							do_emplace(new_buckets, new_info, mutable_value_type(value), info_type(1, hash), dst);
						}
					}
				}
			});
			// Success:
			if constexpr (use_move)
			{
				// Since the fast path was taken above, all buckets are already destructed; mark m_buckets as such.
				m_buckets.mark_count_zero_to_indicate_all_destroyed();
			}
			// Move assignment will invalidate iterators.
			m_buckets = std::move(new_buckets);
			m_shift_bits = new_shift_bits;
		}
		/**	Rehash the buckets to a given bucket count that is smaller than the current bucket count.
		 *	@param new_bucket_count The desired bucket count.
		 */
		void do_rehash_smaller(const size_type new_bucket_count)
		{
			SH_ROBINHOOD_ASSERT(new_bucket_count < m_buckets.get_capacity());

			// If wide, pessimisticly assume that whatever horrible anomaly resulted in going wide before will remain or
			// could happen again post-rehash.
			buckets_type new_buckets(new_bucket_count, m_buckets.get_wide(), m_buckets.get_allocator());
			const bits_type new_shift_bits = shift_bits<hash_result>(new_bucket_count);

			// Rewinding requires that move construction can't throw _again_.
			static_assert(std::is_nothrow_move_constructible_v<mutable_value_type>, "rehash rewind unsupported without noexcept move constructor for mutable_value_type.");
			static_assert(std::is_nothrow_swappable_v<mutable_value_type>, "rehash rewind unsupported without noexcept swap for mutable_value_type.");

			// If rehashing to a smaller size, we need to prepare for the possibility of a widen operation. We'll need
			// to handle get_info in do_emplace each time and take caution that widen allocation may throw a std::bad_alloc.
			// In that case, we'll handle the exception by rewinding new_buckets back into m_buckets before propagating.
			try
			{
				m_buckets.with_info([this, &new_buckets, new_shift_bits](auto* const info)
				{
					const size_type capacity = m_buckets.get_capacity();
					for (size_type index = 0; index < capacity; ++index)
					{
						if (info[index].empty() == false)
						{
							mutable_value_type& value = m_buckets.get_value(index);
							// hasher can safely throw here, even though it shouldn't ever happen.
							const hash_result hash = static_cast<const hasher&>(*this)(value.key());
							const size_type dst = hash_clamp(hash, new_shift_bits);
							do_emplace(new_buckets, std::move(value), wide_info_type(1, hash), dst);
							// Erase value now, so that:
							//   a. If there's an exception thrown, we can do_emplace back into this spot. This is why we
							//      cannot just use m_buckets.destroy(value).
							//   b. Count will be zero and m_buckets can avoid iterating over all values when assigned below.
							m_buckets.erase(info, index);
						}
					}
				});
			}
			catch (...)
			{
				// Rewind.
				m_buckets.with_info([this, &new_buckets](auto* const info)
				{
					using info_type = std::decay_t<decltype(*info)>;
					new_buckets.with_info([this, &new_buckets, info](const auto* const new_info)
					{
						const size_type new_capacity = new_buckets.get_capacity();
						for (size_type index = 0; index < new_capacity; ++index)
						{
							if (new_info[index].empty() == false)
							{
								mutable_value_type& value = new_buckets.get_value(index);
								// hasher cannot safely throw here, but not all are marked noexcept!
								const hash_result hash = static_cast<const hasher&>(*this)(value.key());
								const size_type dst = hash_clamp_with_shift_bits(hash);
								// This is expected to never throw:
								//   a. We know there's an empty place, we made it above in the "try" with erase.
								//	 b. We're not going to widen and it's explicitly disabled: value_temp came out
								//      of m_buckets with the current info_type, it should be able to go back in!
								constexpr bool can_widen = false;
								constexpr typename info_type::distance_type initial_distance = 1;
								do_emplace<info_type, can_widen>(m_buckets, info, std::move(value), info_type(initial_distance, hash), dst);
								// Destruct value now, so that we can get away with mark_count_zero_to_indicate_all_destroyed
								// and won't have to reiterate over new_buckets when it's destructed below.
								new_buckets.destroy(value);
							}
						}
					});
				});
				new_buckets.mark_count_zero_to_indicate_all_destroyed();
				// Propagate exception.
				throw;
			}
			// Success. This move assignment will invalidate iterators.
			m_buckets = std::move(new_buckets);
			m_shift_bits = new_shift_bits;
		}
		/**	Rehash the buckets to a given bucket count.
		 *	@note Does not check if the current and desired bucket count are equal before performing the rehash.
		 *	@details do_rehash_smaller should remain a separate function as its exception handling causes this function to optimize poorly.
		 *	@param new_bucket_count The desired bucket count.
		 */
		void do_rehash(const size_type new_bucket_count)
		{
			if SH_ROBINHOOD_LIKELY(new_bucket_count >= bucket_count())
			{
				do_rehash_larger(new_bucket_count);
			}
			else
			{
				do_rehash_smaller(new_bucket_count);
			}
		}

		/**	Called by do_emplace<narrow_info_type> when widening is impossible and by do_widen_or_rewind_one_and_throw.
		 *	@tparam InfoType The type of Robinhood info being used by buckets.
		 *	@param result_index The index at which do_emplace initially emplaced a value & info.
		 *	@param buckets The Robinhood buckets upon which to operate.
		 *	@param info The info array currently being used by buckets.
		 *	@param value_temp The value to emplace. Either the original value or one that is being swapped into another position.
		 */
		template <typename InfoType>
		void do_rewind_one_if_necessary(const size_type result_index, buckets_type& buckets, InfoType* const info, mutable_value_type& value_temp) const noexcept
		{
			if (result_index != buckets.get_capacity())
			{
				do_erase(buckets, info, result_index);

				// hasher cannot safely throw here, but not all are marked noexcept!
				const hash_result hash = static_cast<const hasher&>(*this)(value_temp.key());
				const size_type dst = hash_clamp_with_shift_bits(hash);
				// This is expected to never throw:
				//   a. We know there's an empty place, we just made it above with do_erase.
				//	 b. We're not going to widen and it's explicitly disabled: value_temp
				//      came out of buckets with the current InfoType, it should be able to
				//      go back in!
				constexpr bool can_widen = false;
				constexpr typename InfoType::distance_type initial_distance = 1;
				do_emplace<InfoType, can_widen>(buckets, info, std::move(value_temp), InfoType(initial_distance, hash), dst);
			}
		}

		/**	Called by do_emplace<narrow_info_type> when at the end of an emplace it is determined that widen must be called.
		 *	@tparam InfoType The type of Robinhood info being used by buckets.
		 *	@param result_index The index at which do_emplace initially emplaced a value & info.
		 *	@param buckets The Robinhood buckets to operate upon.
		 *	@param value_temp The value to emplace. Either the original value or one that is being swapped into another position.
		 */
		void do_widen_or_rewind_one_and_throw(const size_type result_index, buckets_type& buckets, mutable_value_type& value_temp) const
		{
			static_assert(policy_type::constant_width == false);
			SH_ROBINHOOD_ASSERT(buckets.get_wide() == false);
			try
			{
				// Asserts that constant_width == false and that buckets' info is currently narrow.
				buckets.widen();
			}
			catch(std::bad_alloc&)
			{
				// Rewind, if necessary.
				narrow_info_type* const narrow_info = buckets.template get_info<narrow_info_type>();
				do_rewind_one_if_necessary(result_index, buckets, narrow_info, value_temp);
				// Propagate exception.
				throw;
			}
		}

		/**	Called by do_emplace<narrow_info_type> when partway through an emplace it is determined that widen must be called.
		 *	@note Assumes that there must be an empty bucket somewhere in buckets.
		 *	@param initial_index The index at which do_emplace was working before calling do_widen_and_continue_emplace.
		 *	@param result_index The index at which do_emplace initially emplaced a value & info.
		 *	@param buckets The Robinhood buckets to operate upon.
		 *	@param value_temp The value to emplace. Either the original value or one that is being swapped into another position.
		 *	@param info_temp The info to emplace. Either the original info or one that is being swapped into another position.
		 *	@param midpoint_index The optimistic index in buckets at which to do_emplace hoped to emplace the initial value & info.
		 *	@return The emplace result.
		 */
		emplace_result do_widen_and_continue_emplace(const size_type initial_index, size_type result_index, buckets_type& buckets, mutable_value_type& value_temp, wide_info_type info_temp, const size_type midpoint_index) const
		{
			static_assert(policy_type::constant_width == false);
			SH_ROBINHOOD_ASSERT(buckets.get_wide() == false);

			// Will throw on failure, after rewinding & re-emplacing value_temp.
			do_widen_or_rewind_one_and_throw(result_index, buckets, value_temp);

			const size_type capacity = buckets.get_capacity();
			wide_info_type* const wide_info = buckets.template get_info<wide_info_type>();
			SH_ROBINHOOD_ASSERT(info_temp.get_distance() < max_info_distance<wide_info_type>());
			info_temp.push_forward();

			// If true, scanning from midpoint_index to capacity was in-progress.
			// Else, initial_index is less than midpoint_index and the wrap-to-zero has already occurred.
			if (initial_index >= midpoint_index)
			{
				// Relies on an empty info element at buckets[capacity]
				static_assert(buckets_type::info_tail > 0);
				SH_ROBINHOOD_ASSERT(wide_info[capacity].empty());
				for (size_type index = initial_index + 1; /*index != capacity*/; ++index)
				{
					if (wide_info[index].empty())
					{
						if SH_ROBINHOOD_UNLIKELY(index == capacity)
						{
							// We've hit the empty bucket, continue below.
							break;
						}
						else
						{
							static_assert(std::is_nothrow_move_constructible_v<mutable_value_type>, "mutable_value_type excepted to have noexcept move constructor.");
							buckets.emplace(wide_info, index, info_temp, std::move(value_temp));
							return { result_index == capacity ? index : result_index };
						}
					}
					else if (wide_info[index].get_distance() < info_temp.get_distance())
					{
						static_assert(std::is_nothrow_swappable_v<mutable_value_type>, "mutable_value_type excepted to have noexcept swap operation.");
						value_temp.swap(buckets.get_value(index));
						static_assert(std::is_nothrow_swappable_v<wide_info_type>, "wide_info_type excepted to have noexcept swap operation.");
						info_temp.swap(wide_info[index]);
						if (result_index == capacity)
						{
							result_index = index;
						}
					}
					if SH_ROBINHOOD_UNLIKELY(info_temp.get_distance() == max_info_distance<wide_info_type>())
					{
						do_rewind_one_if_necessary(result_index, buckets, wide_info, value_temp);
						throw std::length_error("Robinhood max_distance reached.");
					}
					info_temp.push_forward();
				}
				// Assumption made here that there must be an empty bucket before midpoint:
				for (size_type index = 0; /*index != midpoint_index*/; ++index)
				{
					SH_ROBINHOOD_ASSERT(index < midpoint_index);
					if (wide_info[index].empty())
					{
						static_assert(std::is_nothrow_move_constructible_v<mutable_value_type>, "mutable_value_type excepted to have noexcept move constructor.");
						buckets.emplace(wide_info, index, info_temp, std::move(value_temp));
						return { result_index == capacity ? index : result_index };
					}
					else if (wide_info[index].get_distance() < info_temp.get_distance())
					{
						static_assert(std::is_nothrow_swappable_v<mutable_value_type>, "mutable_value_type excepted to have noexcept swap operation.");
						value_temp.swap(buckets.get_value(index));
						static_assert(std::is_nothrow_swappable_v<wide_info_type>, "wide_info_type excepted to have noexcept swap operation.");
						info_temp.swap(wide_info[index]);
						if (result_index == capacity)
						{
							result_index = index;
						}
					}
					if SH_ROBINHOOD_UNLIKELY(info_temp.get_distance() == wide_info_type::max_distance())
					{
						do_rewind_one_if_necessary(result_index, buckets, wide_info, value_temp);
						throw std::length_error("Robinhood max_distance reached.");
					}
					info_temp.push_forward();
				}
			}
			else
			{
				// Assumption made here that there must be an empty bucket before midpoint:
				for (size_type index = initial_index + 1; /*index != midpoint_index*/; ++index)
				{
					SH_ROBINHOOD_ASSERT(index < midpoint_index);
					if (wide_info[index].empty())
					{
						static_assert(std::is_nothrow_move_constructible_v<mutable_value_type>, "mutable_value_type excepted to have noexcept move constructor.");
						buckets.emplace(wide_info, index, info_temp, std::move(value_temp));
						return { result_index == capacity ? index : result_index };
					}
					else if (wide_info[index].get_distance() < info_temp.get_distance())
					{
						static_assert(std::is_nothrow_swappable_v<mutable_value_type>, "mutable_value_type excepted to have noexcept swap operation.");
						value_temp.swap(buckets.get_value(index));
						static_assert(std::is_nothrow_swappable_v<wide_info_type>, "wide_info_type excepted to have noexcept swap operation.");
						info_temp.swap(wide_info[index]);
						if (result_index == capacity)
						{
							result_index = index;
						}
					}
					if SH_ROBINHOOD_UNLIKELY(info_temp.get_distance() == wide_info_type::max_distance())
					{
						do_rewind_one_if_necessary(result_index, buckets, wide_info, value_temp);
						throw std::length_error("Robinhood max_distance reached.");
					}
					info_temp.push_forward();
				}
			}
			SH_ROBINHOOD_ASSERT(result_index != capacity);
			return { result_index };
		}

		/**	Emplace a given value & info into the given buckets.
		 *	@note Assumes that there must be an empty bucket somewhere in buckets.
		 *	@tparam InfoType The type of Robinhood info being used by buckets.
		 *	@tparam CanWiden If widening the Robinhood info data is an option prior to distance overflow. Defaults to true if the policy is not constant width and InfoType is narrow_info_type.
The type of Robinhood info being used by buckets.
		 *	@param buckets The buckets in which to perform the emplace operation.
		 *	@param info The info array currently being used by buckets.
		 *	@param value_temp The value to emplace.
		 *	@param info_temp The info to emplace.
		 *	@param midpoint_index The optimistic index in buckets at which to emplace the value & info.
		 *	@return The emplace result.
		 */
		template <typename InfoType, bool CanWiden = policy_type::constant_width == false && std::is_same_v<InfoType, wide_info_type> == false>
		emplace_result do_emplace(buckets_type& buckets, InfoType* const info, mutable_value_type&& value_temp, InfoType info_temp, const size_type midpoint_index) const
		{
			const size_type capacity = buckets.get_capacity();
			size_type result_index = capacity;
			SH_ROBINHOOD_ASSERT(info_temp.get_distance() > 0);
			// Relies on an empty info element at buckets[capacity]
			static_assert(buckets_type::info_tail > 0);
			SH_ROBINHOOD_ASSERT(info[capacity].empty());
			for (size_type index = midpoint_index; /*index != capacity*/; ++index)
			{
				if (info[index].empty())
				{
					if SH_ROBINHOOD_UNLIKELY(index == capacity)
					{
						// We've hit the empty bucket, continue below.
						break;
					}
					else
					{
						static_assert(std::is_nothrow_move_constructible_v<mutable_value_type>, "mutable_value_type excepted to have noexcept move constructor.");
						buckets.emplace(info, index, info_temp, std::move(value_temp));
						return { result_index == capacity ? index : result_index };
					}
				}
				else if (info[index].get_distance() < info_temp.get_distance())
				{
					static_assert(std::is_nothrow_swappable_v<mutable_value_type>, "mutable_value_type excepted to have noexcept swap operation.");
					value_temp.swap(buckets.get_value(index));
					static_assert(std::is_nothrow_swappable_v<InfoType>, "InfoType excepted to have noexcept swap operation.");
					info_temp.swap(info[index]);
					if (result_index == capacity)
					{
						result_index = index;
					}
				}
				if SH_ROBINHOOD_UNLIKELY(info_temp.get_distance() == max_info_distance<InfoType>())
				{
					if constexpr (CanWiden)
					{
						return do_widen_and_continue_emplace(index, result_index, buckets, value_temp, info_temp, midpoint_index);
					}
					else
					{
						do_rewind_one_if_necessary(result_index, buckets, info, value_temp);
						throw std::length_error("Robinhood max_distance reached.");
					}
				}
				info_temp.push_forward();
			}
			// Assumption made here that there must be an empty bucket before midpoint:
			for (size_type index = 0; /*index != midpoint_index*/; ++index)
			{
				SH_ROBINHOOD_ASSERT(index < midpoint_index);
				if (info[index].empty())
				{
					static_assert(std::is_nothrow_move_constructible_v<mutable_value_type>, "mutable_value_type excepted to have noexcept move constructor.");
					buckets.emplace(info, index, info_temp, std::move(value_temp));
					return { result_index == capacity ? index : result_index };
				}
				else if (info[index].get_distance() < info_temp.get_distance())
				{
					static_assert(std::is_nothrow_swappable_v<mutable_value_type>, "mutable_value_type excepted to have noexcept swap operation.");
					value_temp.swap(buckets.get_value(index));
					static_assert(std::is_nothrow_swappable_v<InfoType>, "InfoType excepted to have noexcept swap operation.");
					info_temp.swap(info[index]);
					if (result_index == capacity)
					{
						result_index = index;
					}
				}
				if SH_ROBINHOOD_UNLIKELY(info_temp.get_distance() == max_info_distance<InfoType>())
				{
					if constexpr (CanWiden)
					{
						return do_widen_and_continue_emplace(index, result_index, buckets, value_temp, info_temp, midpoint_index);
					}
					else
					{
						do_rewind_one_if_necessary(result_index, buckets, info, value_temp);
						throw std::length_error("Robinhood max_distance reached.");
					}
				}
				info_temp.push_forward();
			}
			SH_ROBINHOOD_ASSERT(result_index != capacity);
			return { result_index };
		}

		/**	Emplace a given value & info into the given buckets.
		 *	@param buckets The buckets in which to perform the emplace operation.
		 *	@param value_temp The value to emplace.
		 *	@param info_temp The info to emplace.
		 *	@param midpoint_index The optimistic index in buckets at which to emplace the value & info.
		 *	@return The emplace_result.
		 */
		emplace_result do_emplace(buckets_type& buckets, mutable_value_type&& value_temp, wide_info_type info_temp, const size_type midpoint_index) const
		{
			if constexpr (policy_type::constant_width)
			{
				if SH_ROBINHOOD_UNLIKELY(info_temp.get_distance() >= narrow_info_type::max_distance())
				{
					throw std::length_error("Robinhood max_distance reached.");
				}
				// Fall through to do_emplace at bottom (narrow_info_type == wide_info_type for constant_width).
			}
			else if SH_ROBINHOOD_LIKELY(buckets.get_wide() == false)
			{
				if SH_ROBINHOOD_UNLIKELY(info_temp.get_distance() > narrow_info_type::max_distance())
				{
					// Will throw on failure.
					buckets.widen();
					// Fall through to do_emplace at bottom (info is now wide_info_type).
				}
				else
				{
					auto* const narrow_info = buckets.template get_info<narrow_info_type>();
					return do_emplace<narrow_info_type>(buckets, narrow_info, std::move(value_temp), info_temp, midpoint_index);
				}
			}
			auto* const wide_info = buckets.template get_info<wide_info_type>();
			return do_emplace<wide_info_type>(buckets, wide_info, std::move(value_temp), info_temp, midpoint_index);
		}

	public:
		/**	Emplace a value constructed from the given arguments & info into m_buckets.
		 *	@param info_temp The info to emplace.
		 *	@param midpoint_index The optimistic index in buckets at which to emplace the value & info.
		 *	@param args The arguments from which to construct the value to be emplaced.
		 *	@return The emplace_result.
		 */
		template <typename... Args>
		emplace_result do_emplace(wide_info_type info_temp, size_type midpoint_index, Args&&... args)
		{
			// A throw during construction is allowed as this & m_buckets haven't been modified at this point.
			mutable_value_type value_temp(std::forward<Args>(args)...);
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			// Invalidate iterators on emplace. Do it once here rather than multiple times in buckets::emplace.
			m_buckets.invalidate_iterators();
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
			if SH_ROBINHOOD_UNLIKELY(load_factor_with_given_size_current_capacity(size() + 1) > m_max_load_factor)
			{
				// Rehash from zero to default_bucket_count other to twice the current size.
				const size_type current_capacity = m_buckets.get_capacity();
				const size_type desired_capacity = current_capacity == 0
					? default_bucket_count
					: (current_capacity << 1u);

				// do_rehash_larger may throw.
				do_rehash_larger(desired_capacity);

				// hasher can safely throw here, even though it shouldn't ever happen.
				const hash_result hash = static_cast<const hasher&>(*this)(value_temp.key());
				midpoint_index = hash_clamp_with_shift_bits(hash);
				info_temp = wide_info_type(1, hash);
			}
			return do_emplace(m_buckets, std::move(value_temp), info_temp, midpoint_index);
		}

	private:
		/**	Erase an index from the given buckets & info.
		 *	@tparam InfoType The type of Robinhood info being used by buckets.
		 *	@param buckets The buckets from which to erase the given index.
		 *	@param info The info array currently being used by buckets.
		 *	@param pos_index The index of the value & info to erase.
		 */
		template <typename InfoType>
		static void do_erase(buckets_type& buckets, InfoType* const info, const size_type pos_index) noexcept
		{
			const size_type capacity = buckets.get_capacity();
			size_type last = pos_index;
			buckets.erase(info, last);
			// Relies on an empty info element at buckets[capacity]
			static_assert(buckets_type::info_tail > 0);
			SH_ROBINHOOD_ASSERT(info[capacity].empty());
			for (size_type index = last + 1; /*index != capacity*/; ++index)
			{
				static_assert(empty_distance_v<typename InfoType::distance_type> <= 1,
					"The result of get_distance() when empty() is true must compare as less-than 1.");
				if (info[index].get_distance() <= 1)
				{
					// We've hit the empty bucket, continue below.
					if SH_ROBINHOOD_UNLIKELY(index == capacity)
					{
						break;
					}
					else
					{
						return;
					}
				}
				else
				{
					buckets.move_back(info, last, index);
					last = index;
				}
			}
			// We know there is an empty bucket at pos, we can rely on that as a
			// stopping condition in this loop.
			for (size_type index = 0; /*index != pos_index*/; ++index)
			{
				SH_ROBINHOOD_ASSERT(index <= pos_index || info[index].empty());
				static_assert(empty_distance_v<typename InfoType::distance_type> <= 1,
					"The result of get_distance() when empty() is true must compare as less-than 1.");
				if (info[index].get_distance() <= 1)
				{
					// We've hit the empty bucket, exit.
					return;
				}
				else
				{
					buckets.move_back(info, last, index);
					last = index;
				}
			}
		}

	public:
		/**	Erase an iterator from m_buckets.
		 *	@param pos The iterator of the value & info to erase.
		 */
		void do_erase(const const_iterator& pos) noexcept
		{
			SH_ROBINHOOD_ASSERT(pos.m_buckets == &m_buckets);
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			SH_ROBINHOOD_ASSERT(m_buckets.check_iterator_debug(pos.m_index, pos.m_debug));
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
			m_buckets.with_info([this, index = pos.m_index](auto* const info)
			{
				do_erase(m_buckets, info, index);
			});
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			// Invalidate iterators on erase.
			m_buckets.invalidate_iterators();
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
		}
		/**	Erase a range of iterators [first, last) from m_buckets.
		 *	@param first The first iterator to erase.
		 *	@param last The iterator at which to stop erasing.
		 */
		void do_erase(const const_iterator& first, const const_iterator& last) noexcept
		{
			SH_ROBINHOOD_ASSERT(first.m_buckets == &m_buckets);
			SH_ROBINHOOD_ASSERT(last.m_buckets == &m_buckets);
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			SH_ROBINHOOD_ASSERT(m_buckets.check_iterator_debug(first.m_index, first.m_debug));
			SH_ROBINHOOD_ASSERT(m_buckets.check_iterator_debug(last.m_index, last.m_debug));
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
			if (first != last)
			{
				// Erasing a range with this Robinhood table is tricky:
				//   a. The last iterator -- unless it's end -- will not remain
				//      valid after earlier iterators are erased.
				//   b. Additionally, even if the end index is "static",
				//      entries could unwrap from the beginning of buckets to fall
				//      before last as earlier entries are erased. This would
				//      result in too many entries being deleted.
				//   c. That also means that right-to-left is not an option. If
				//      the unwrapping occurred, too few entries would be deleted.
				//   d. The best option seems to be two-pass:
				//     i.  Count the number of non-empty entries between [first,
				//         last).
				//     ii. Starting at first, delete that number of entries.
				m_buckets.with_info([this, left = first.m_index, right = last.m_index](auto* const info)
				{
					// Ascertain iterator validity. Left cannot equal capacity (end) as passed first != last
					// condition above unless right is invalid itself.
					SH_ROBINHOOD_ASSERT(left < m_buckets.get_capacity());
					SH_ROBINHOOD_ASSERT(info[left].empty() == false);
					SH_ROBINHOOD_ASSERT(right <= m_buckets.get_capacity());
					SH_ROBINHOOD_ASSERT(right == m_buckets.get_capacity() || info[right].empty() == false);
					// As given iterators must be valid, left cannot be empty.
					size_type to_erase = 1;
					for (size_type index = left + 1; index < right; ++index)
					{
						if (info[index].empty() == false)
						{
							++to_erase;
						}
					}
					// Erase the discovered count from left-to-right.
					size_type erased = 1;
					size_type index = left;
					do_erase(m_buckets, info, index);
					while (erased != to_erase)
					{
						// As entries can move_back, don't increase index unless it's empty.
						while (info[index].empty())
						{
							++index;
							SH_ROBINHOOD_ASSERT(index < m_buckets.get_capacity());
						}
						do_erase(m_buckets, info, index);
						++erased;
					};
				});
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
				// Invalidate iterators on erase.
				m_buckets.invalidate_iterators();
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
			}
		}
		/**	Erase an iterator from m_buckets and return the next iterator.
		 *	@param pos The iterator of the value & info to erase.
		 *	@return The iterator immediately following the erased element.
		 */
		iterator do_erase_continue(const const_iterator& pos) noexcept
		{
			SH_ROBINHOOD_ASSERT(pos.m_buckets == &m_buckets);
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			SH_ROBINHOOD_ASSERT(m_buckets.check_iterator_debug(pos.m_index, pos.m_debug));
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
			const auto result = m_buckets.with_info([this, pos_index = pos.m_index](auto* const info) -> size_type
			{
				do_erase(m_buckets, info, pos_index);
				const size_type capacity = m_buckets.get_capacity();
				for (size_type index = pos_index; index < capacity; ++index)
				{
					if (info[index].empty() == false)
					{
						return index;
					}
				}
				return capacity;
			});
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			// Invalidate iterators on erase.
			m_buckets.invalidate_iterators();
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
			return index(result);
		}
		/**	Erase a range of iterators [first, last) from m_buckets.
		 *	@param first The first iterator to erase.
		 *	@param last The iterator at which to stop erasing.
		 *	@return An iterator equivalent to last (at the end if end, otherwise referring to the same key/value).
		 */
		iterator do_erase_continue(const const_iterator& first, const const_iterator& last) noexcept
		{
			SH_ROBINHOOD_ASSERT(first.m_buckets == &m_buckets);
			SH_ROBINHOOD_ASSERT(last.m_buckets == &m_buckets);
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
			SH_ROBINHOOD_ASSERT(m_buckets.check_iterator_debug(first.m_index, first.m_debug));
			SH_ROBINHOOD_ASSERT(m_buckets.check_iterator_debug(last.m_index, last.m_debug));
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
			if (first == last)
			{
				// No erase done, no iterator invalidation necessary.
				return index(last.m_index);
			}
			else
			{
				// Erasing a range with this Robinhood table is tricky:
				//   a. The last iterator -- unless it's end -- will not remain
				//      valid after earlier iterators are erased.
				//   b. Additionally, even if the end index is "static",
				//      entries could unwrap from the beginning of buckets to fall
				//      before last as earlier entries are erased. This would
				//      result in too many entries being deleted.
				//   c. That also means that right-to-left is not an option. If
				//      the unwrapping occurred, too few entries would be deleted.
				//   d. The best option seems to be two-pass:
				//     i.  Count the number of non-empty entries between [first,
				//         last).
				//     ii. Starting at first, delete that number of entries.
				const size_type result = m_buckets.with_info([this, left = first.m_index, right = last.m_index](auto* const info) -> size_type
				{
					const size_type capacity = m_buckets.get_capacity();
					// Ascertain iterator validity. Left cannot equal capacity (end) as passed first != last
					// condition above unless right is invalid itself.
					SH_ROBINHOOD_ASSERT(left < capacity);
					SH_ROBINHOOD_ASSERT(info[left].empty() == false);
					SH_ROBINHOOD_ASSERT(right <= capacity);
					SH_ROBINHOOD_ASSERT(right == capacity || info[right].empty() == false);
					// As given iterators must be valid, left cannot be empty.
					size_type to_erase = 1;
					for (size_type index = left + 1; index < right; ++index)
					{
						if (info[index].empty() == false)
						{
							++to_erase;
						}
					}
					// Erase the discovered count from left-to-right.
					size_type erased = 1;
					size_type index = left;
					do_erase(m_buckets, info, index);
					while (erased != to_erase)
					{
						// As entries can move_back, don't increase index unless it's empty.
						while (info[index].empty())
						{
							++index;
							SH_ROBINHOOD_ASSERT(index < capacity);
						}
						do_erase(m_buckets, info, index);
						++erased;
					};
					// Increment index to the next non-empty entry or the end.
					while (info[index].empty() && index < capacity)
					{
						++index;
					}
					return index;
				});
#ifdef SH_ROBINHOOD_DEBUG_ITERATOR
				// Invalidate iterators on erase.
				m_buckets.invalidate_iterators();
#endif // SH_ROBINHOOD_DEBUG_ITERATOR
				return index(result);
			}
		}

	private:
		/**	Attempt to find a value that matches the given hash & arguments.
		 *	@details Since args are never constructed into a mutable_value_type, key_equal should either accept args verbatim or support a very fast construction operation. For example, if a Args=<const char*> and key_equal has only operator()(const std::string, const std::string) this will result in a std::string construction for every key_equal::operator() call! This may be negligible for low-coincidence lookups, but the worst case is fairly poor.
		 *	@tparam InfoType The type of Robinhood info being used by buckets.
		 *	@tparam Args The arguments to pass to the left-hand parameter of key_equal.
		 *	@param info The info array currently being used by buckets.
		 *	@param hash The hash_result of passing args to the hasher.
		 *	@param args The arguments that would construct the value to find.
		 *	@return The find result.
		 */
		template <typename InfoType, typename... Args>
		find_result do_find(const InfoType* const info, const hash_result hash, Args&&... args) const noexcept
		{
			const size_type capacity = m_buckets.get_capacity();
			const size_type bucket_index = hash_clamp_with_shift_bits(hash);
			constexpr typename InfoType::distance_type initial_distance = 1;
			wide_info_type info_temp(initial_distance, hash);
			// Relies on an empty info element at m_buckets[capacity]
			static_assert(buckets_type::info_tail > 0);
			SH_ROBINHOOD_ASSERT(info[capacity].empty());
			for (size_type index = bucket_index; /*index != capacity*/; ++index, info_temp.push_forward())
			{
				static_assert(empty_distance_v<typename InfoType::distance_type> == std::numeric_limits<typename InfoType::distance_type>::min(),
					"The result of get_distance() when empty() is true must compare as less-than get_distance() when empty() is false.");
				if (info[index].get_distance() < info_temp.get_distance())
				{
					if SH_ROBINHOOD_UNLIKELY(index == capacity)
					{
						// We've hit the empty bucket, continue below.
						break;
					}
					else
					{
						return find_result{ index, info_temp.get_distance(), false };
					}
				}
				else if(info[index].cached_hash_equal(info_temp)
					&& static_cast<const key_equal&>(*this)(args..., m_buckets.get_value(index).key()))
				{
					return find_result{ index, info_temp.get_distance(), true };
				}
			}
			// The first path will be followed when there are no buckets or all buckets are filled.
			if SH_ROBINHOOD_UNLIKELY(m_buckets.get_count() == capacity)
			{
				// If there are no buckets, this will immediately fail.
				for (size_type index = 0; index != bucket_index; ++index, info_temp.push_forward())
				{
					static_assert(empty_distance_v<typename InfoType::distance_type> == std::numeric_limits<typename InfoType::distance_type>::min(),
						"The result of get_distance() when empty() is true must compare as less-than get_distance() when empty() is false.");
					if (info[index].get_distance() < info_temp.get_distance())
					{
						return find_result{ index, info_temp.get_distance(), false };
					}
					else if(info[index].cached_hash_equal(info_temp)
						&& static_cast<const key_equal&>(*this)(args..., m_buckets.get_value(index).key()))
					{
						return find_result{ index, info_temp.get_distance(), true };
					}
				}
			}
			else
			{
				// If there is an empty bucket and we didn't find it in the earlier
				// loop, we can rely on that as a stopping condition in this loop.
				for (size_type index = 0; ; ++index, info_temp.push_forward())
				{
					SH_ROBINHOOD_ASSERT(index <= bucket_index || info[index].empty());
					static_assert(empty_distance_v<typename InfoType::distance_type> == std::numeric_limits<typename InfoType::distance_type>::min(),
						"The result of get_distance() when empty() is true must compare as less-than get_distance() when empty() is false.");
					if (info[index].get_distance() < info_temp.get_distance())
					{
						return find_result{ index, info_temp.get_distance(), false };
					}
					else if(info[index].cached_hash_equal(info_temp)
						&& static_cast<const key_equal&>(*this)(args..., m_buckets.get_value(index).key()))
					{
						return find_result{ index, info_temp.get_distance(), true };
					}
				}
			}
			return find_result{ bucket_index, info_temp.get_distance(), false };
		}

	public:
		/**	Attempt to find a value that matches the given hash & arguments.
		 *	@tparam Args The arguments to pass to the left-hand parameter of key_equal.
		 *	@param hash The hash_result of passing args to the hasher.
		 *	@param args The arguments that would construct the value to find.
		 *	@return The find result.
		 */
		template <typename... Args>
		find_result do_find(const hash_result hash, Args&&... args) const noexcept
		{
			return m_buckets.with_info([this, hash, &args...](const auto* const info) noexcept -> find_result
			{
				return do_find(info, hash, std::forward<Args>(args)...);
			});
		}

		template <typename KeyArg>
		constexpr static std::conditional_t<(is_transparent || std::is_same_v<key_type, std::decay_t<std::remove_reference_t<KeyArg>>>), KeyArg&&, key_type>
			do_key(KeyArg&& key_arg) noexcept
		{
			if constexpr (false == is_transparent && false == std::is_same_v<key_type, std::decay_t<std::remove_reference_t<KeyArg>>>)
			{
				if constexpr (is_transparent_v<hasher>)
				{
					SH_ROBINHOOD_WARN("robinhood::do_key casting to key_type despite hasher::is_transparent.");
				}
				if constexpr (is_transparent_v<key_equal>)
				{
					SH_ROBINHOOD_WARN("robinhood::do_key casting to key_type despite key_equal::is_transparent.");
				}
			}
			// Allow key_arg to cast to key_type implicitly, if able.
			return std::forward<KeyArg>(key_arg);
		}

	protected:
		constexpr size_type hash_clamp_with_shift_bits(const hash_result hash) const noexcept
		{
			return hash_clamp(hash, m_shift_bits);
		}
		constexpr float load_factor_with_given_size_current_capacity(const size_type size) const noexcept
		{
			return size / std::max<float>(std::numeric_limits<float>::min(), bucket_count());
		}

		/**	The container of keys and any values.
		 *	@note Handles the key/value-associated Robinhood info, an element count, and allocator.
		 */
		buckets_type m_buckets;

		/**	The maximum load factor to maintain on reserve or emplace.
		 *	@note Defaults to default_load_factor.
		 */
		float m_max_load_factor;

		/**	Used to clamp hash_result values to the power-of-two capacity of buckets.
		 *	@note Updated on rehash (do_rehash_larger and do_rehash_smaller).
		 */
		bits_type m_shift_bits;
	};

} // namespace sh::robinhood

#endif
