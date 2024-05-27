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

#ifndef INC_SH__OPENMAP_HPP
#define INC_SH__OPENMAP_HPP

#include "robinhood.hpp"

#include <algorithm>
#include <cassert>
#include <functional>
#include <initializer_list>
#include <stdexcept>
#include <utility>

namespace sh
{

namespace robinhood
{
	/**	A key/value container with an immutable key and mutable value akin to std::pair.
	 *	@tparam K The key or first member's type.
	 *	@tparam V The value or second member's type.
	 *	@note Provides both a key and value member function to its respective contained values. Provides a mutable key member function to its friend, mutable_key_value_pair.
	 *	@details Relies on const_cast to expose first mutably to its friends. Hence, if const_key_value_pair is itself declared const, such access is undefined behavior.
	 */
	template <typename K, typename V>
	class const_key_value_pair final
	{
	public:
		using key_type = K;
		using value_type = V;
		using first_type = const key_type;
		using second_type = value_type;

		/**	std::pair-like accessible (constant) first member.
		 */
		first_type first;
		/**	std::pair-like accessible second member.
		 */
		second_type second;

		/**	Construct from a given key and value.
		 *	@param key The value to forward to first (or key).
		 *	@param value The value to forward to second (or value).
		 */
		template <typename KeyArg, typename ValueArg>
		constexpr const_key_value_pair(KeyArg&& key, ValueArg&& value)
			noexcept(std::is_nothrow_constructible_v<key_type, KeyArg&&>
				&& std::is_nothrow_constructible_v<value_type, ValueArg&&>)
			: first{ std::forward<KeyArg>(key) }
			, second{ std::forward<ValueArg>(value) }
		{ }
		/**	Construct from a pair's members.
		 *	@param keyvalue The pair from which to accept first as first (or key) and second as second (or value).
		 */
		template <typename KeyArg, typename ValueArg>
		constexpr const_key_value_pair(const std::pair<KeyArg, ValueArg>& keyvalue)
			noexcept(std::is_nothrow_constructible_v<key_type, KeyArg>
				&& std::is_nothrow_constructible_v<value_type, ValueArg>)
			: first{ keyvalue.first }
			, second{ keyvalue.second }
		{ }
		/**	Move construct from a pair's members.
		 *	@param keyvalue The pair from which to accept std::move(first) as first (or key) and std::move(second) as second (or value).
		 */
		template <typename KeyArg, typename ValueArg>
		constexpr const_key_value_pair(std::pair<KeyArg, ValueArg>&& keyvalue)
			noexcept(std::is_nothrow_constructible_v<key_type, KeyArg&&>
				&& std::is_nothrow_constructible_v<value_type, ValueArg&&>)
			: first{ std::move(keyvalue.first) }
			, second{ std::move(keyvalue.second) }
		{ }
		const_key_value_pair() = default;
		const_key_value_pair(const const_key_value_pair&) = default;
		const_key_value_pair(const_key_value_pair&& other)
			noexcept(std::is_nothrow_move_constructible_v<key_type>
				&& std::is_nothrow_move_constructible_v<value_type>)
			: first{ std::move(other.key()) }
			, second{ std::move(other.second) }
		{ }
		~const_key_value_pair() = default;

		/**	Provides const access to the contained key.
		 *	@return A const reference to first.
		 */
		constexpr const key_type& key() const noexcept
		{
			return first;
		}
		/**	Provides const access to the contained value.
		 *	@return A const reference to second.
		 */
		constexpr const value_type& value() const noexcept
		{
			return second;
		}
		/**	Provides mutable access to the contained value.
		 *	@return A non-const reference to second.
		 */
		constexpr value_type& value() noexcept
		{
			return second;
		}

	private:
		/**	The only container type allowed to mutate const_key_value_pair.
		 */
		template <typename K2, typename V2> friend class mutable_key_value_pair;

		/**	Provides copy assignment access to const_key_value_pair's friend, mutable_key_value_pair.
		 *	@param other The const_key_value_pair to assign.
		 *	@return A reference to this.
		 */
		const_key_value_pair& operator=(const const_key_value_pair& other)
			noexcept(std::is_nothrow_copy_assignable_v<key_type>
				&& std::is_nothrow_copy_assignable_v<value_type>)
		{
			key() = other.key();
			second = other.second;
			return *this;
		}

		/**	Provides move assignment access to const_key_value_pair's friend, mutable_key_value_pair.
		 *	@param other The const_key_value_pair to assign.
		 *	@return A reference to this.
		 */
		const_key_value_pair& operator=(const_key_value_pair&& other)
			noexcept(std::is_nothrow_move_assignable_v<key_type>
				&& std::is_nothrow_move_assignable_v<value_type>)
		{
			key() = std::move(other.key());
			second = std::move(other.second);
			return *this;
		}

		/**	Provides mutable access to key only to const_key_value_pair's friend, mutable_key_value_pair.
		 *	@return A non-const reference to the contained key.
		 */
		constexpr key_type& key() noexcept
		{
			return const_cast<key_type&>(first);
		}
	};

	/**	A key/value container with a mutable key that can expose a constant-key inner value in a one-way fashion.
	 *	@tparam K The key type.
	 *	@tparam V The value type.
	 */
	template <typename K, typename V>
	class mutable_key_value_pair final
	{
	public:
		using key_type = K;
		using value_type = V;

		/**	Cast to the internal const_key_value_pair.
		 *	@return The internal const_key_value_pair.
		 */
		constexpr explicit operator const_key_value_pair<K,V>&() noexcept
		{
			static_assert(sizeof(mutable_key_value_pair) == sizeof(const_key_value_pair<K,V>));
			return m_keyvalue;
		}
		/**	Cast to the internal const_key_value_pair.
		 *	@return The internal const_key_value_pair.
		 */
		constexpr explicit operator const const_key_value_pair<K,V>&() const noexcept
		{
			static_assert(sizeof(mutable_key_value_pair) == sizeof(const_key_value_pair<K,V>));
			return m_keyvalue;
		}

		/**	Construct from the given arguments.
		 *	@param args The arguments from which to construct const_key_value_pair.
		 *	@details The first argument is expected to be the key and the second the value.
		 */
		template <typename... Args>
		constexpr explicit mutable_key_value_pair(Args&&... args)
			noexcept(std::is_nothrow_constructible_v<const_key_value_pair<key_type, value_type>, Args...>)
			: m_keyvalue{ std::forward<Args>(args)... }
		{ }
		mutable_key_value_pair() = default;
		mutable_key_value_pair(const mutable_key_value_pair&) = default;
		mutable_key_value_pair(mutable_key_value_pair&&) = default;
		~mutable_key_value_pair() = default;

		mutable_key_value_pair& operator=(const mutable_key_value_pair&) = default;
		mutable_key_value_pair& operator=(mutable_key_value_pair&&) = default;

		/**	Provides mutable access to the contained key.
		 *	@return A non-const reference to the key.
		 */
		constexpr key_type& key() noexcept
		{
			return m_keyvalue.key();
		}
		/**	Provides const access to the contained key.
		 *	@return A const reference to the key.
		 */
		constexpr const key_type& key() const noexcept
		{
			return m_keyvalue.key();
		}
		/**	Provides mutable access to the contained value.
		 *	@return A non-const reference to the value.
		 */
		constexpr value_type& value() noexcept
		{
			return m_keyvalue.value();
		}
		/**	Provides const access to the contained value.
		 *	@return A const reference to the value.
		 */
		constexpr const value_type& value() const noexcept
		{
			return m_keyvalue.value();
		}

		/**	Swap this key and value with another mutable_key_value_pair's.
		 *	@param other The other mutable_key_value_pair with which to swap key and value.
		 */
		void swap(mutable_key_value_pair& other)
			noexcept(std::is_nothrow_swappable_v<key_type>
				&& std::is_nothrow_swappable_v<value_type>)
		{
			using std::swap;
			swap(key(), other.key());
			swap(value(), other.value());
		}
		/**	Swap the key and value of two mutable_key_value_pair objects.
		 *	@param lhs A mutable_key_value_pair.
		 *	@param rhs Another mutable_key_value_pair.
		 */
		friend void swap(mutable_key_value_pair& lhs, mutable_key_value_pair& rhs)
			noexcept(std::is_nothrow_swappable_v<key_type>
				&& std::is_nothrow_swappable_v<value_type>)
		{
			lhs.swap(rhs);
		}

	private:
		/**	An std::pair-like object with an immutable key to all except mutable_key_value_pair.
		 */
		const_key_value_pair<key_type, value_type> m_keyvalue;
	};

	/**	A namespace-scoped is_trivial specialization for mutable_key_value_pair.
	 *	@tparam K The key type.
	 *	@tparam V The value type.
	 *	@details Will mark mutable_key_value_pair as satisfying is trivial if its key and value types are trivially constructible, destructible, and copyable.
	 */
	template <typename K, typename V>
	struct is_trivial <mutable_key_value_pair<K, V>> final
	{
		static constexpr bool value = std::is_trivially_constructible_v<K>
			&& std::is_trivially_constructible_v<V>
			&& std::is_trivially_destructible_v<K>
			&& std::is_trivially_destructible_v<V>
			&& std::is_trivially_copyable_v<K>
			&& std::is_trivially_copyable_v<V>;
	};

	/**	Used to implement a std::unordered_map-style, key/value-pair Robinhood hashtable.
	 *	@tparam Key The type of key used in the map.
	 *	@tparam T The type of value used in the map.
	 *	@tparam SizeType The size_type of the map buckets.
	 */
	template <typename Key, typename T, typename SizeType>
	using map_policy = robinhood::policy<Key, const_key_value_pair<Key, T>, mutable_key_value_pair<Key, T>, SizeType>;
} // namespace robinhood

template <typename Key,
	typename T,
	typename Hash = std::hash<Key>,
	typename KeyEqual = std::equal_to<Key>,
	typename Allocator = std::allocator<std::pair<const Key, T>>,
	typename SizeType = robinhood::default_size_type>
class openmap : private robinhood::hashtable<robinhood::map_policy<Key, T, SizeType>, Hash, KeyEqual, Allocator>
{
private:
	using hashtable_type = robinhood::hashtable<robinhood::map_policy<Key, T, SizeType>, Hash, KeyEqual, Allocator>;
	using typename hashtable_type::find_result;
	using typename hashtable_type::hash_result;
	using typename hashtable_type::wide_info_type;

public:
	using key_type = Key;
	using mapped_type = T;
	using hasher = Hash;
	using key_equal = KeyEqual;
	using allocator_type = Allocator;
	using value_type = typename hashtable_type::exposed_value_type;
	using size_type = SizeType;
	using difference_type = std::ptrdiff_t;
	using reference = value_type&;
	using const_reference = const value_type&;
	using pointer = value_type*;
	using const_pointer = const value_type*;
	using local_iterator = typename hashtable_type::exposed_value_type*;
	using const_local_iterator = const typename hashtable_type::exposed_value_type*;
	using typename hashtable_type::iterator;
	using typename hashtable_type::const_iterator;

	openmap() noexcept(std::is_nothrow_constructible_v<allocator_type>);
	explicit openmap(const allocator_type& alloc) noexcept;
	explicit openmap(size_type bucket_count, const hasher& hash = hasher(), const key_equal& equal = key_equal(), const allocator_type& alloc = allocator_type());
	openmap(size_type bucket_count, const hasher& hash, const allocator_type& alloc);
	openmap(size_type bucket_count, const allocator_type& alloc);
	openmap(const openmap& other);
	openmap(const openmap& other, const allocator_type& alloc);
	openmap(openmap&& other) noexcept(std::is_nothrow_constructible_v<allocator_type>);
	openmap(openmap&& other, const allocator_type& alloc) noexcept(typename hashtable_type::allocator_traits::is_always_equal());
	template <typename InputIt>
	openmap(InputIt first, InputIt last, size_type bucket_count = robinhood::default_bucket_count, const hasher& hash = hasher(), const key_equal& equal = key_equal(), const allocator_type& alloc = allocator_type());
	template <typename InputIt>
	openmap(InputIt first, InputIt last, size_type bucket_count, const hasher& hash, const allocator_type& alloc);
	template <typename InputIt>
	openmap(InputIt first, InputIt last, size_type bucket_count, const allocator_type& alloc);
	openmap(std::initializer_list<std::pair<key_type, mapped_type>> init, size_type bucket_count = robinhood::default_bucket_count, const hasher& hash = hasher(), const key_equal& equal = key_equal(), const allocator_type& alloc = allocator_type());
	openmap(std::initializer_list<std::pair<key_type, mapped_type>> init, size_type bucket_count, const hasher& hash, const allocator_type& alloc);
	openmap(std::initializer_list<std::pair<key_type, mapped_type>> init, size_type bucket_count, const allocator_type& alloc);
	~openmap() = default;

	openmap& operator=(const openmap& other);
	openmap& operator=(openmap&& other) noexcept;
	using hashtable_type::get_allocator;

	using hashtable_type::bucket;
	using hashtable_type::empty;
	using hashtable_type::size;
	using hashtable_type::max_size;
	using hashtable_type::bucket_count;
	using hashtable_type::max_bucket_count;
	using hashtable_type::bucket_size;
	using hashtable_type::load_factor;
	using hashtable_type::max_load_factor;
	using hashtable_type::hash_function;
	using hashtable_type::key_eq;

	constexpr iterator begin();
	constexpr iterator end();
	constexpr const_iterator begin() const;
	constexpr const_iterator end() const;
	constexpr const_iterator cbegin() const;
	constexpr const_iterator cend() const;

	constexpr local_iterator begin(size_type n);
	constexpr local_iterator end(size_type n);
	constexpr const_local_iterator begin(size_type n) const;
	constexpr const_local_iterator end(size_type n) const;
	constexpr const_local_iterator cbegin(size_type n) const;
	constexpr const_local_iterator cend(size_type n) const;

	using hashtable_type::clear;
	using hashtable_type::swap;
	using hashtable_type::reserve;
	using hashtable_type::rehash;
	void swap(openmap& other) noexcept;

	inline std::pair<iterator, bool> insert(const value_type& value);
	inline std::pair<iterator, bool> insert(value_type&& value);
	inline iterator insert(const_iterator hint, const value_type& value);
	inline iterator insert(const_iterator hint, value_type&& value);
	template <typename InputIt>
	void insert(InputIt first, InputIt last);
	void insert(std::initializer_list<std::pair<key_type, mapped_type>> ilist);
	template <typename KeyArg, typename ValueArg>
	std::pair<iterator, bool> insert_or_assign(KeyArg&& key_arg, ValueArg&& value);
	template <typename KeyArg, typename ValueArg>
	iterator insert_or_assign(const_iterator hint, KeyArg&& key_arg, ValueArg&& value);
	template <typename... Args>
	std::pair<iterator, bool> emplace(Args&&... args);
	template <typename... Args>
	iterator emplace_hint(const_iterator hint, Args&&... args);
	template <typename KeyArg, typename... Args>
	std::pair<iterator, bool> try_emplace(KeyArg&& key_arg, Args&&... args);
	template <typename KeyArg, typename... Args>
	iterator try_emplace_hint(const_iterator hint, KeyArg&& key_arg, Args&&... args);

	template <typename KeyArg>
	size_type erase(KeyArg&& key_arg);
	void erase(iterator pos);
	void erase(const_iterator pos);
	void erase(const_iterator first, const_iterator last);
	iterator erase_continue(const_iterator pos);
	iterator erase_continue(const_iterator first, const_iterator last);

	template <typename KeyArg>
	mapped_type& operator[](KeyArg&& key_arg);
	template <typename KeyArg>
	inline mapped_type& at(KeyArg&& key_arg);
	template <typename KeyArg>
	inline const mapped_type& at(KeyArg&& key_arg) const;

	template <typename KeyArg>
	inline size_type count(KeyArg&& key_arg) const;
	template <typename KeyArg>
	inline bool contains(KeyArg&& key_arg) const;
	template <typename KeyArg>
	iterator find(KeyArg&& key_arg);
	template <typename KeyArg>
	inline const_iterator find(KeyArg&& key_arg) const;
	template <typename KeyArg>
	inline std::pair<iterator, iterator> equal_range(KeyArg&& key_arg);
	template <typename KeyArg>
	inline std::pair<const_iterator, const_iterator> equal_range(KeyArg&& key_arg) const;
};

template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::openmap() noexcept(std::is_nothrow_constructible_v<allocator_type>)
	: hashtable_type{ allocator_type() }
{ }
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::openmap(const allocator_type& alloc) noexcept
	: hashtable_type{ alloc }
{ }
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::openmap(const size_type bucket_count, const hasher& hash, const key_equal& equal, const allocator_type& alloc)
	: hashtable_type{ bucket_count, hash, equal, alloc }
{ }
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::openmap(const size_type bucket_count, const hasher& hash, const allocator_type& alloc)
	: hashtable_type{ bucket_count, hash, key_equal(), alloc }
{ }
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::openmap(const size_type bucket_count, const allocator_type& alloc)
	: hashtable_type{ bucket_count, hasher(), key_equal(), alloc }
{ }
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::openmap(const openmap& other)
	: hashtable_type{ other }
{ }
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::openmap(const openmap& other, const allocator_type& alloc)
	: hashtable_type{ other, alloc }
{ }
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::openmap(openmap&& other) noexcept(std::is_nothrow_constructible_v<allocator_type>)
	: hashtable_type{ std::move(other) }
{ }
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::openmap(openmap&& other, const allocator_type& alloc) noexcept(typename hashtable_type::allocator_traits::is_always_equal())
	: hashtable_type{ std::move(other), alloc }
{ }
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename InputIt>
openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::openmap(const InputIt first, const InputIt last, const size_type bucket_count, const hasher& hash, const key_equal& equal, const allocator_type& alloc)
	: hashtable_type{ bucket_count, hash, equal, alloc }
{
	insert(first, last);
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename InputIt>
openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::openmap(const InputIt first, const InputIt last, const size_type bucket_count, const hasher& hash, const allocator_type& alloc)
	: openmap{ first, last, bucket_count, hash, key_equal(), alloc }
{ }
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename InputIt>
openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::openmap(const InputIt first, const InputIt last, const size_type bucket_count, const allocator_type& alloc)
	: hashtable_type{ first, last, bucket_count, hasher(), key_equal(), alloc }
{ }
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::openmap(std::initializer_list<std::pair<key_type, mapped_type>> init, const size_type bucket_count, const hasher& hash, const key_equal& equal, const allocator_type& alloc)
	: hashtable_type{ bucket_count, hash, equal, alloc }
{
	insert(std::move(init));
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::openmap(std::initializer_list<std::pair<key_type, mapped_type>> init, const size_type bucket_count, const hasher& hash, const allocator_type& alloc)
	: openmap{ std::move(init), bucket_count, hash, key_equal(), alloc }
{ }
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::openmap(std::initializer_list<std::pair<key_type, mapped_type>> init, const size_type bucket_count, const allocator_type& alloc)
	: openmap{ std::move(init), bucket_count, hasher(), key_equal(), alloc }
{ }

template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::operator=(const openmap& other) -> openmap&
{
	this->hashtable_type::operator=(other);
	return *this;
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::operator=(openmap&& other) noexcept -> openmap&
{
	this->hashtable_type::operator=(std::move(other));
	return *this;
}

template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
constexpr auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::begin() -> iterator
{
	return iterator{ this->hashtable_type::begin() };
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
constexpr auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::end() -> iterator
{
	return iterator{ this->hashtable_type::end() };
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
constexpr auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::begin() const -> const_iterator
{
	return cbegin();
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
constexpr auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::end() const -> const_iterator
{
	return cend();
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
constexpr auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::cbegin() const -> const_iterator
{
	return const_cast<openmap&>(*this).begin();
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
constexpr auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::cend() const -> const_iterator
{
	return const_cast<openmap&>(*this).end();
}

template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
constexpr auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::begin(const size_type n) -> local_iterator
{
	return local_iterator{ this->hashtable_type::begin(n) };
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
constexpr auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::end(const size_type n) -> local_iterator
{
	return local_iterator{ this->hashtable_type::end(n) };
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
constexpr auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::begin(const size_type n) const -> const_local_iterator
{
	return cbegin(n);
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
constexpr auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::end(const size_type n) const -> const_local_iterator
{
	return cend(n);
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
constexpr auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::cbegin(const size_type n) const -> const_local_iterator
{
	const auto it = this->hashtable_type::begin(n);
	return const_local_iterator{ it ? &static_cast<const value_type&>(*it) : nullptr };
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
constexpr auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::cend(const size_type n) const -> const_local_iterator
{
	const auto it = this->hashtable_type::end(n);
	return const_local_iterator{ it ? &static_cast<const value_type&>(*it) : nullptr };
}

template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
void openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::swap(openmap& other) noexcept
{
	this->hashtable_type::swap(other);
}

template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::insert(const value_type& value) -> std::pair<iterator, bool>
{
	return try_emplace(value.first, value.second);
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::insert(value_type&& value) -> std::pair<iterator, bool>
{
	return try_emplace(std::move(value.first), std::move(value.second));
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::insert(const_iterator hint, const value_type& value) -> iterator
{
	return try_emplace_hint(hint, std::move(value.first), value.second);
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::insert(const_iterator hint, value_type&& value) -> iterator
{
	return try_emplace_hint(hint, std::move(value.first), std::move(value.second));
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename InputIt>
void openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::insert(InputIt first, const InputIt last)
{
	while (first != last)
	{
		insert(*(first++));
	}
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
void openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::insert(std::initializer_list<std::pair<key_type, mapped_type>> ilist)
{
	for (const auto& value : ilist)
	{
		insert(value);
	}
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename KeyArg, typename ValueArg>
auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::insert_or_assign(KeyArg&& key_arg, ValueArg&& value) -> std::pair<iterator, bool>
{
	auto&& key = this->hashtable_type::do_key(std::forward<KeyArg>(key_arg));
	const hash_result hash = static_cast<const hasher&>(*this)(key);
	const find_result result = this->hashtable_type::do_find(hash, key);
	if (result.m_found)
	{
		const auto result_it = this->hashtable_type::index(result.m_index);
		result_it->value() = std::forward<ValueArg>(value);
		return std::make_pair(iterator{ result_it }, false);
	}
	return std::make_pair(
		iterator{
			this->hashtable_type::index(
				this->hashtable_type::do_emplace(
					wide_info_type{ result.m_distance, hash },
					result.m_index,
					std::forward<decltype(key)>(key),
					std::forward<ValueArg>(value)).m_index
			)
		},
		true);
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename KeyArg, typename ValueArg>
auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::insert_or_assign(const_iterator hint, KeyArg&& key_arg, ValueArg&& value) -> iterator
{
	return insert_or_assign(std::forward<KeyArg>(key_arg), std::forward<ValueArg>(value)).first;
}

template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename... Args>
auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::emplace(Args&&... args) -> std::pair<iterator, bool>
{
	if constexpr (sizeof...(Args) == 2)
	{
		return try_emplace(std::forward<Args>(args)...);
	}
	else
	{
		value_type value{ std::forward<Args>(args)... };
		return try_emplace(std::move(value.first), std::move(value.second));
	}
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename... Args>
auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::emplace_hint(const const_iterator hint, Args&&... args) -> iterator
{
	return emplace(std::forward<Args>(args)...).first;
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename KeyArg, typename... Args>
auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::try_emplace(KeyArg&& key_arg, Args&&... args) -> std::pair<iterator, bool>
{
	auto&& key = this->hashtable_type::do_key(std::forward<KeyArg>(key_arg));
	const hash_result hash = static_cast<const hasher&>(*this)(key);
	const find_result result = this->hashtable_type::do_find(hash, key);
	return std::make_pair(
		iterator{
			this->hashtable_type::index(
				result.m_found
					? result.m_index
					: this->hashtable_type::do_emplace(
						wide_info_type{ result.m_distance, hash },
						result.m_index,
						std::forward<decltype(key)>(key),
						std::forward<Args>(args)...).m_index
			)
		},
		result.m_found == false);
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename KeyArg, typename... Args>
auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::try_emplace_hint(const const_iterator hint, KeyArg&& key_arg, Args&&... args) -> iterator
{
	return try_emplace(std::forward<KeyArg>(key_arg), std::forward<Args>(args)...).first;
}

template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename KeyArg>
auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::erase(KeyArg&& key_arg) -> size_type
{
	const iterator it = find(std::forward<KeyArg>(key_arg));
	if (it == end())
	{
		return 0;
	}
	erase(it);
	return 1;
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
void openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::erase(const iterator pos)
{
	erase(const_iterator{ pos });
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
void openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::erase(const const_iterator pos)
{
	this->hashtable_type::do_erase(pos);
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
void openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::erase(const const_iterator first, const const_iterator last)
{
	this->hashtable_type::do_erase(first, last);
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::erase_continue(const const_iterator pos) -> iterator
{
	return this->hashtable_type::do_erase_continue(pos);
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::erase_continue(const const_iterator first, const const_iterator last) -> iterator
{
	return this->hashtable_type::do_erase_continue(first, last);
}

template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename KeyArg>
auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::operator[](KeyArg&& key_arg) -> mapped_type&
{
	auto&& key = this->hashtable_type::do_key(std::forward<KeyArg>(key_arg));
	const hash_result hash = static_cast<const hasher&>(*this)(key);
	find_result result = this->hashtable_type::do_find(hash, key);
	if (result.m_found == false)
	{
		result.m_index = this->hashtable_type::do_emplace(
			wide_info_type{ result.m_distance, hash },
			result.m_index,
			std::forward<decltype(key)>(key),
			mapped_type()).m_index;
	}
	return this->hashtable_type::index(result.m_index)->value();
}

template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename KeyArg>
auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::at(KeyArg&& key_arg) -> mapped_type&
{
	const iterator it = find(std::forward<KeyArg>(key_arg));
	if SH_ROBINHOOD_UNLIKELY(it == end())
	{
		throw std::out_of_range("openmap::at");
	}
	return it->second;
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename KeyArg>
auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::at(KeyArg&& key_arg) const -> const mapped_type&
{
	const const_iterator it = find(std::forward<KeyArg>(key_arg));
	if SH_ROBINHOOD_UNLIKELY(it == end())
	{
		throw std::out_of_range("openmap::at");
	}
	return it->second;
}

template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename KeyArg>
auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::count(KeyArg&& key_arg) const -> size_type
{
	return find(std::forward<KeyArg&&>(key_arg)) != end() ? 1 : 0;
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename KeyArg>
bool openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::contains(KeyArg&& key_arg) const
{
	return find(std::forward<KeyArg&&>(key_arg)) != end();
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename KeyArg>
auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::find(KeyArg&& key_arg) -> iterator
{
	auto&& key = this->hashtable_type::do_key(std::forward<KeyArg>(key_arg));
	const hash_result hash = static_cast<const hasher&>(*this)(key);
	const find_result result = this->hashtable_type::do_find(hash, std::forward<decltype(key)>(key));
	return result.m_found
		? iterator{ this->hashtable_type::index(result.m_index) }
		: end();
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename KeyArg>
auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::find(KeyArg&& key_arg) const -> const_iterator
{
	auto&& key = this->hashtable_type::do_key(std::forward<KeyArg>(key_arg));
	const hash_result hash = static_cast<const hasher&>(*this)(key);
	const find_result result = this->hashtable_type::do_find(hash, std::forward<decltype(key)>(key));
	return result.m_found
		? const_iterator{ this->hashtable_type::index(result.m_index) }
		: cend();
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename KeyArg>
auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::equal_range(KeyArg&& key_arg) -> std::pair<iterator, iterator>
{
	const iterator it = find(std::forward<KeyArg>(key_arg));
	return std::make_pair(it, it);
}
template <typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename KeyArg>
auto openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>::equal_range(KeyArg&& key_arg) const -> std::pair<const_iterator, const_iterator>
{
	const const_iterator it = find(std::forward<KeyArg>(key_arg));
	return std::make_pair(it, it);
}

} // namespace sh

#endif
