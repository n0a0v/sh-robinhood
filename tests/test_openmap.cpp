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

#include <gtest/gtest.h>

#include <sh/openmap.hpp>

#include <climits>
#include <string>
#include <type_traits>

using sh::openmap;
using sh::robinhood::fibonacci_fraction;
using sh::robinhood::log2ui;

constexpr bool DEBUG_VERBOSE = false;

namespace
{
	// Lifted implementation detail to double-check if bucketing has changed:
	template <typename H, typename T>
	constexpr auto hash_clamp(H&& hasher, T&& key, const std::size_t bucket_count)
	{
		auto hash_result = hasher(key);
		const auto shift_bits = sizeof(hash_result)*CHAR_BIT - log2ui(bucket_count);
		hash_result ^= hash_result >> shift_bits;
		return (fibonacci_fraction<decltype(hash_result)>() * hash_result) >> shift_bits;
	}

	template <typename HT>
	void dump_openmap(const HT& table)
	{
		std::cerr << "size: " << table.size() << '\n';
		std::cerr << "bucket_count: " << table.bucket_count() << '\n';
		for (std::size_t n = 0; n < table.bucket_count(); ++n)
		{
			std::cerr << "bucket " << n << " of bucket_size " << table.bucket_size(n) << ":\n";
			for (auto it = table.begin(n); it != table.end(n); ++it)
			{
				std::cerr << "\tkey(" << it->first << ") = value(" << it->second << ')'
					<< " hash(" << table.hash_function()(it->first) << " => " << table.bucket(it->first) << ")"
					<< std::endl;
			}
		}
	}

	struct allocations
	{
		std::size_t m_current{ 0 };
		std::size_t m_peak{ 0 };
		std::size_t m_allocate_calls{ 0 };
		std::size_t m_deallocate_calls{ 0 };
		std::size_t m_construct_calls{ 0 };
		std::size_t m_destroy_calls{ 0 };

		void reset()
		{
			*this = allocations{};
		}
		friend std::ostream& operator<<(std::ostream& ostr, const allocations& all)
		{
			return ostr << "current " << all.m_current << " bytes"
				", peak " << all.m_peak << " bytes"
				", allocate calls " << all.m_allocate_calls <<
				", deallocate calls " << all.m_deallocate_calls <<
				", construct calls " << all.m_construct_calls <<
				", destroy calls " << all.m_destroy_calls;
		}
	};

	template <typename T>
	struct typed_allocations : allocations
	{
		static allocations& get()
		{
			static allocations instance;
			return instance;
		}
	};

	using general_allocations = typed_allocations<void>;

	template <typename T>
	class counted_allocator : private std::allocator<T>
	{
	public:
		using allocator_traits = std::allocator_traits<std::allocator<T>>;
		using value_type = typename allocator_traits::value_type;
		using size_type = typename allocator_traits::size_type;
		using difference_type = typename allocator_traits::difference_type;

		template <typename U>
		struct rebind
		{
			using other = counted_allocator<U>;
		};

		counted_allocator() = default;
		counted_allocator(const counted_allocator& other) = default;
		counted_allocator(counted_allocator&& other) noexcept = default;
		counted_allocator& operator=(const counted_allocator& other) = default;
		counted_allocator& operator=(counted_allocator&& other) noexcept = default;

		template <typename U>
		explicit counted_allocator(const counted_allocator<U>& other) noexcept
		{ }
		template <typename U>
		explicit counted_allocator(counted_allocator<U>&& other) noexcept
		{ }

		[[nodiscard]] constexpr T* allocate(const std::size_t n)
		{
			general_allocations::get().m_current += sizeof(T) * n;
			general_allocations::get().m_allocate_calls += 1;
			general_allocations::get().m_peak = std::max(general_allocations::get().m_peak, general_allocations::get().m_current);

			typed_allocations<T>::get().m_current += n;
			typed_allocations<T>::get().m_allocate_calls += 1;
			typed_allocations<T>::get().m_peak = std::max(typed_allocations<T>::get().m_peak, typed_allocations<T>::get().m_current);
			return this->std::allocator<T>::allocate(n);
		}
		constexpr void deallocate(T* const p, const std::size_t n)
		{
			general_allocations::get().m_current -= sizeof(T) * n;
			general_allocations::get().m_deallocate_calls += 1;
			typed_allocations<T>::get().m_current -= n;
			typed_allocations<T>::get().m_deallocate_calls += 1;
			this->std::allocator<T>::deallocate(p, n);
		}
		template <typename U, typename... Args>
		void construct(U* const p, Args&&... args)
		{
			this->std::allocator<T>::template construct<U>(p, std::forward<Args>(args)...);
			if constexpr (std::is_trivially_destructible_v<U> == false)
			{
				general_allocations::get().m_construct_calls += 1;
				typed_allocations<T>::get().m_construct_calls += 1;
			}
		}
		template <typename U>
		void destroy(U* const p)
		{
			if constexpr (std::is_trivially_destructible_v<U> == false)
			{
				general_allocations::get().m_destroy_calls += 1;
				typed_allocations<T>::get().m_destroy_calls += 1;
			}
			this->std::allocator<T>::template destroy<U>(p);
		}
	};

	template <typename T, bool Propagate = true>
	struct stateful_allocator : private counted_allocator<T>
	{
		using allocator_traits = std::allocator_traits<counted_allocator<T>>;
		using value_type = typename allocator_traits::value_type;
		using size_type = typename allocator_traits::size_type;
		using difference_type = typename allocator_traits::difference_type;
		using counted_allocator<T>::allocate;
		using counted_allocator<T>::deallocate;
		using counted_allocator<T>::construct;
		using counted_allocator<T>::destroy;

		template <typename U>
		struct rebind
		{
			using other = stateful_allocator<U, Propagate>;
		};

		using propagate_on_container_copy_assignment = std::bool_constant<Propagate>;
		using propagate_on_container_move_assignment = std::bool_constant<Propagate>;
		using propagate_on_container_swap = std::bool_constant<Propagate>;

		stateful_allocator() = delete;
		explicit constexpr stateful_allocator(const int id) noexcept
			: m_id(id)
		{ }
		stateful_allocator(const stateful_allocator& other) = default;
		stateful_allocator(stateful_allocator&& other) noexcept = default;
		stateful_allocator& operator=(const stateful_allocator& other) = default;
		stateful_allocator& operator=(stateful_allocator&& other) noexcept = default;

		template <typename U, bool P>
		explicit stateful_allocator(const stateful_allocator<U, P>& other) noexcept
			: m_id(other.m_id)
		{ }
		template <typename U, bool P>
		explicit stateful_allocator(stateful_allocator<U, P>&& other) noexcept
			: m_id(other.m_id)
		{ }

		constexpr bool operator==(const stateful_allocator& other) const noexcept
		{
			return m_id == other.m_id;
		}
		constexpr bool operator!=(const stateful_allocator& other) const noexcept
		{
			return m_id != other.m_id;
		}

		int m_id;
	};

	template <typename T>
	struct unreliable_allocator : private counted_allocator<T>
	{
		using allocator_traits = std::allocator_traits<counted_allocator<T>>;
		using value_type = typename allocator_traits::value_type;
		using size_type = typename allocator_traits::size_type;
		using difference_type = typename allocator_traits::difference_type;

		template <typename U>
		struct rebind
		{
			using other = unreliable_allocator<U>;
		};

		using propagate_on_container_copy_assignment = std::true_type;
		using propagate_on_container_move_assignment = std::true_type;
		using propagate_on_container_swap = std::true_type;

		explicit unreliable_allocator(const bool& can_allocate) noexcept
			: m_can_allocate(&can_allocate)
		{ }
		unreliable_allocator(const unreliable_allocator& other) = default;
		unreliable_allocator(unreliable_allocator&& other) noexcept = default;
		unreliable_allocator& operator=(const unreliable_allocator& other) = default;
		unreliable_allocator& operator=(unreliable_allocator&& other) noexcept = default;

		template <typename U>
		explicit unreliable_allocator(const unreliable_allocator<U>& other) noexcept
			: m_can_allocate(other.m_can_allocate)
		{ }
		template <typename U>
		explicit unreliable_allocator(unreliable_allocator<U>&& other) noexcept
			: m_can_allocate(other.m_can_allocate)
		{ }

		constexpr bool operator==(const unreliable_allocator& other) const noexcept
		{
			return m_can_allocate == other.m_can_allocate;
		}
		constexpr bool operator!=(const unreliable_allocator& other) const noexcept
		{
			return m_can_allocate != other.m_can_allocate;
		}

		[[nodiscard]] constexpr T* allocate(const std::size_t n)
		{
			if (*m_can_allocate == false)
			{
				throw std::bad_alloc();
			}
			return this->counted_allocator<T>::allocate(n);
		}
		using counted_allocator<T>::deallocate;
		using counted_allocator<T>::construct;
		using counted_allocator<T>::destroy;

		const bool* m_can_allocate;
	};

	template <typename T>
	class transparent_hash : private std::hash<T>
	{
	public:
		using is_transparent = void;

		transparent_hash(std::uint32_t& used_normally, std::uint32_t& used_transparently) noexcept
			: m_used_normally{ &used_normally }
			, m_used_transparently{ &used_transparently }
		{ }

		constexpr std::size_t operator()(const T& value) const noexcept
		{
			++(*m_used_normally);
			return this->std::hash<T>::operator()(value);
		}
		template <typename U>
		constexpr std::size_t operator()(const U& value) const noexcept
		{
			++(*m_used_transparently);
			return this->std::hash<T>::operator()(value);
		}

	private:
		std::uint32_t* m_used_normally;
		std::uint32_t* m_used_transparently;
	};

	// Bad hash:
	template <typename T>
	struct thousands_hash
	{
		constexpr std::size_t operator()(const T& value) const noexcept
		{
			return value / 1000;
		}
	};

	// Fixture
	class sh_openmap : public ::testing::Test
	{
	protected:
		void SetUp() override
		{
			general_allocations::get() = allocations{};
			ASSERT_EQ(0u, general_allocations::get().m_current);
			ASSERT_EQ(0u, general_allocations::get().m_allocate_calls);
			ASSERT_EQ(0u, general_allocations::get().m_deallocate_calls);
			ASSERT_EQ(0u, general_allocations::get().m_construct_calls);
			ASSERT_EQ(0u, general_allocations::get().m_destroy_calls);
		}
		void TearDown() override
		{
			EXPECT_EQ(0u, general_allocations::get().m_current);
			EXPECT_EQ(general_allocations::get().m_allocate_calls, general_allocations::get().m_deallocate_calls);
			EXPECT_EQ(general_allocations::get().m_construct_calls, general_allocations::get().m_destroy_calls);
			if constexpr (DEBUG_VERBOSE)
			{
				std::cout << '\t' << general_allocations::get() << std::endl;
			}
		}
	};

	template <typename Key,
		typename T,
		typename Hash = std::hash<Key>,
		typename KeyEqual = std::equal_to<Key>,
		typename Allocator = counted_allocator<std::pair<const Key, T>>,
		typename SizeType = sh::robinhood::default_size_type>
	using counted_openmap = openmap<Key, T, Hash, KeyEqual, Allocator, SizeType>;
} // anonymous namespace

TEST_F(sh_openmap, ctor_default)
{
	counted_openmap<int, std::string> x;
	EXPECT_TRUE(x.empty());
	EXPECT_EQ(x.size(), 0u);
}
TEST_F(sh_openmap, ctor_alloc)
{
	using allocator_type = stateful_allocator<std::pair<int, std::string>>;
	openmap<int, std::string, std::hash<int>, std::equal_to<int>, allocator_type> x(allocator_type(123));
	EXPECT_TRUE(x.empty());
	EXPECT_EQ(x.size(), 0u);
}
TEST_F(sh_openmap, ctor_buckets)
{
	counted_openmap<int, std::string> x(7);
	EXPECT_TRUE(x.empty());
	EXPECT_EQ(x.size(), 0u);
	EXPECT_GE(x.bucket_count(), 7u);
}
TEST_F(sh_openmap, ctor_copy)
{
	counted_openmap<int, std::string> x(7);
	x.try_emplace(1, "one");
	ASSERT_FALSE(x.empty());
	ASSERT_EQ(x.size(), 1u);
	ASSERT_GE(x.bucket_count(), 1u);
	ASSERT_TRUE(x.contains(1));

	counted_openmap<int, std::string> y(x);
	EXPECT_FALSE(y.empty());
	EXPECT_EQ(y.size(), 1u);
	EXPECT_EQ(y.bucket_count(), x.bucket_count());
	EXPECT_EQ(y.at(1), "one");
}
TEST_F(sh_openmap, ctor_copy_alloc)
{
	using allocator_type = stateful_allocator<std::pair<int, std::string>>;
	using map_type = openmap<int, std::string, std::hash<int>, std::equal_to<int>, allocator_type>;
	{
		constexpr int alloc_id = 123;
		map_type x(7, allocator_type(alloc_id));
		x.try_emplace(1, "one");
		ASSERT_FALSE(x.empty());
		ASSERT_EQ(x.size(), 1u);
		ASSERT_GE(x.bucket_count(), 1u);
		ASSERT_TRUE(x.contains(1));

		map_type y(x);
		EXPECT_EQ(y.get_allocator(), x.get_allocator());
		EXPECT_FALSE(y.empty());
		EXPECT_EQ(y.size(), 1u);
		EXPECT_EQ(y.bucket_count(), x.bucket_count());
		EXPECT_EQ(y.at(1), "one");
	}
	{
		constexpr int x_alloc_id = 123, y_alloc_id = 456;
		map_type x(7, allocator_type(x_alloc_id));
		x.try_emplace(1, "one");
		ASSERT_FALSE(x.empty());
		ASSERT_EQ(x.size(), 1u);
		ASSERT_GE(x.bucket_count(), 1u);
		ASSERT_TRUE(x.contains(1));

		map_type y(x, allocator_type(y_alloc_id));
		EXPECT_NE(y.get_allocator(), x.get_allocator());
		EXPECT_FALSE(y.empty());
		EXPECT_EQ(y.size(), 1u);
		EXPECT_EQ(y.bucket_count(), x.bucket_count());
		EXPECT_EQ(y.at(1), "one");
	}
}
TEST_F(sh_openmap, ctor_move)
{
	counted_openmap<int, std::string> x(7);
	x.try_emplace(1, "one");
	ASSERT_FALSE(x.empty());
	ASSERT_EQ(x.size(), 1u);
	const auto x_bucket_count = x.bucket_count();
	ASSERT_GE(x_bucket_count, 1u);
	ASSERT_TRUE(x.contains(1));

	counted_openmap<int, std::string> y(std::move(x));
	EXPECT_FALSE(y.empty());
	EXPECT_EQ(y.size(), 1u);
	EXPECT_EQ(y.bucket_count(), x_bucket_count);
	EXPECT_EQ(y.at(1), "one");
}
TEST_F(sh_openmap, ctor_move_iterator)
{
	counted_openmap<int, std::string> x = { { 1, "one" } };
	ASSERT_FALSE(x.empty());
	ASSERT_EQ(x.size(), 1u);

	const auto it = x.begin();
	ASSERT_NE(it, x.end());
	ASSERT_EQ(it->first, 1u);
	ASSERT_EQ(it->second, "one");

	counted_openmap<int, std::string> y{ std::move(x) };
	EXPECT_TRUE(x.empty());
	EXPECT_EQ(x.size(), 0u);
	EXPECT_FALSE(y.empty());
	EXPECT_EQ(y.size(), 1u);

	EXPECT_EQ(it, y.begin());
	ASSERT_NE(it, y.end());
	EXPECT_EQ(it->first, 1u);
	EXPECT_EQ(it->second, "one");
}
TEST_F(sh_openmap, ctor_move_alloc)
{
	using allocator_type = stateful_allocator<std::pair<int, std::string>>;
	using map_type = openmap<int, std::string, std::hash<int>, std::equal_to<int>, allocator_type>;
	{
		constexpr int alloc_id = 123;
		map_type x(7, allocator_type(alloc_id));
		x.try_emplace(1, "one");
		ASSERT_FALSE(x.empty());
		ASSERT_EQ(x.size(), 1u);
		const auto x_bucket_count = x.bucket_count();
		ASSERT_GE(x_bucket_count, 1u);
		ASSERT_TRUE(x.contains(1));

		map_type y(std::move(x));
		EXPECT_EQ(y.get_allocator(), x.get_allocator());
		EXPECT_FALSE(y.empty());
		EXPECT_EQ(y.size(), 1u);
		EXPECT_EQ(y.bucket_count(), x_bucket_count);
		EXPECT_EQ(y.at(1), "one");
	}
	{
		constexpr int x_alloc_id = 123, y_alloc_id = 456;
		map_type x(7, allocator_type(x_alloc_id));
		x.try_emplace(1, "one");
		ASSERT_FALSE(x.empty());
		ASSERT_EQ(x.size(), 1u);
		const auto x_bucket_count = x.bucket_count();
		ASSERT_GE(x_bucket_count, 1u);
		ASSERT_TRUE(x.contains(1));

		map_type y(std::move(x), allocator_type(y_alloc_id));
		EXPECT_NE(y.get_allocator(), x.get_allocator());
		EXPECT_FALSE(y.empty());
		EXPECT_EQ(y.size(), 1u);
		EXPECT_EQ(y.bucket_count(), x_bucket_count);
		EXPECT_EQ(y.at(1), "one");
	}
}
TEST_F(sh_openmap, ctor_range)
{
	const std::vector<std::pair<int, std::string>> values = {
		{ 1, "one" },
		{ 2, "two" },
		{ 3, "three" },
	};

	counted_openmap<int, std::string> x(values.begin(), values.end());
	EXPECT_FALSE(x.empty());
	EXPECT_EQ(x.size(), values.size());
	EXPECT_GE(x.bucket_count(), 1u);
	for (const auto& [key, value] : values)
	{
		EXPECT_TRUE(x.contains(key));
		EXPECT_EQ(x.at(key), value);
	}
}
TEST_F(sh_openmap, ctor_initializer_list)
{
	const std::initializer_list<std::pair<int, std::string>> ilist = {
		{ 1, "one" },
		{ 2, "two" },
		{ 3, "three" },
	};

	counted_openmap<int, std::string> x(ilist.begin(), ilist.end());
	EXPECT_FALSE(x.empty());
	EXPECT_EQ(x.size(), ilist.size());
	EXPECT_GE(x.bucket_count(), 1u);
	for (const auto& item : ilist)
	{
		EXPECT_TRUE(x.contains(item.first));
		EXPECT_EQ(x.at(item.first), item.second);
	}
}
TEST_F(sh_openmap, operator_assign)
{
	{
		using map_type = counted_openmap<int, std::string>;
		map_type x(7);
		x.try_emplace(1, "one");
		ASSERT_FALSE(x.empty());
		ASSERT_EQ(x.size(), 1u);
		ASSERT_GE(x.bucket_count(), 1u);
		ASSERT_TRUE(x.contains(1));

		map_type y;
		y = x;
		EXPECT_FALSE(y.empty());
		EXPECT_EQ(y.size(), 1u);
		EXPECT_EQ(y.bucket_count(), x.bucket_count());
		EXPECT_EQ(y.at(1), "one");
	}
	{
		using map_type = openmap<int, int>;
		map_type x(7);
		x.try_emplace(1, 1111);
		ASSERT_FALSE(x.empty());
		ASSERT_EQ(x.size(), 1u);
		ASSERT_GE(x.bucket_count(), 1u);
		ASSERT_TRUE(x.contains(1));

		map_type y(7);
		y = x;
		EXPECT_FALSE(y.empty());
		EXPECT_EQ(y.size(), 1u);
		EXPECT_EQ(y.bucket_count(), x.bucket_count());
		EXPECT_EQ(y.at(1), 1111);
	}
	{
		using map_type = counted_openmap<int, std::string>;
		map_type x(7);
		x.try_emplace(1, "one");
		ASSERT_FALSE(x.empty());
		ASSERT_EQ(x.size(), 1u);
		ASSERT_GE(x.bucket_count(), 1u);
		ASSERT_TRUE(x.contains(1));

		x = x;
		EXPECT_FALSE(x.empty());
		EXPECT_EQ(x.size(), 1u);
		EXPECT_GE(x.bucket_count(), 1u);
		EXPECT_TRUE(x.contains(1));
	}
}
TEST_F(sh_openmap, operator_assign_iterator)
{
	counted_openmap<int, std::string> y, x = { { 1, "one" } };
	ASSERT_FALSE(x.empty());
	ASSERT_EQ(x.size(), 1u);
	ASSERT_TRUE(y.empty());
	ASSERT_EQ(y.size(), 0u);

	const auto it = x.begin();
	ASSERT_NE(it, x.end());
	ASSERT_EQ(it->first, 1u);
	ASSERT_EQ(it->second, "one");

	y = std::move(x);
	EXPECT_TRUE(x.empty());
	EXPECT_EQ(x.size(), 0u);
	EXPECT_FALSE(y.empty());
	EXPECT_EQ(y.size(), 1u);

	EXPECT_EQ(it, y.begin());
	ASSERT_NE(it, y.end());
	EXPECT_EQ(it->first, 1u);
	EXPECT_EQ(it->second, "one");
}
TEST_F(sh_openmap, operator_assign_alloc)
{
	{
		constexpr bool propagate = true;
		using allocator_type = stateful_allocator<std::pair<int, std::string>, propagate>;
		using map_type = openmap<int, std::string, std::hash<int>, std::equal_to<int>, allocator_type>;
		{
			constexpr int alloc_id = 123;
			map_type x{7, allocator_type(alloc_id)};
			x.try_emplace(1, "one");
			ASSERT_FALSE(x.empty());
			ASSERT_EQ(x.size(), 1u);
			ASSERT_GE(x.bucket_count(), 1u);
			ASSERT_TRUE(x.contains(1));

			map_type y{allocator_type(alloc_id)};
			y = x;
			ASSERT_EQ(y.get_allocator(), x.get_allocator());
			EXPECT_FALSE(y.empty());
			EXPECT_EQ(y.size(), 1u);
			EXPECT_EQ(y.bucket_count(), x.bucket_count());
			EXPECT_EQ(y.at(1), "one");
		}
		{
			constexpr int x_alloc_id = 123, y_alloc_id = 456;
			map_type x{7, allocator_type(x_alloc_id)};
			x.try_emplace(1, "one");
			ASSERT_FALSE(x.empty());
			ASSERT_EQ(x.size(), 1u);
			ASSERT_GE(x.bucket_count(), 1u);
			ASSERT_TRUE(x.contains(1));

			map_type y{allocator_type(y_alloc_id)};
			y = x;
			ASSERT_EQ(y.get_allocator(), x.get_allocator());
			EXPECT_FALSE(y.empty());
			EXPECT_EQ(y.size(), 1u);
			EXPECT_EQ(y.bucket_count(), x.bucket_count());
			EXPECT_EQ(y.at(1), "one");
		}
		{
			constexpr int alloc_id = 123;
			map_type x{7, allocator_type(alloc_id)};
			x.try_emplace(1, "one");
			ASSERT_FALSE(x.empty());
			ASSERT_EQ(x.size(), 1u);
			ASSERT_GE(x.bucket_count(), 1u);
			ASSERT_TRUE(x.contains(1));

			map_type y{7, allocator_type(alloc_id)};
			y = x;
			ASSERT_EQ(y.get_allocator(), x.get_allocator());
			EXPECT_FALSE(y.empty());
			EXPECT_EQ(y.size(), 1u);
			EXPECT_EQ(y.bucket_count(), x.bucket_count());
			EXPECT_EQ(y.at(1), "one");
		}
	}
	{
		constexpr bool propagate = false;
		using allocator_type = stateful_allocator<std::pair<int, std::string>, propagate>;
		using map_type = openmap<int, std::string, std::hash<int>, std::equal_to<int>, allocator_type>;
		{
			constexpr int alloc_id = 123;
			map_type x{7, allocator_type(alloc_id)};
			x.try_emplace(1, "one");
			ASSERT_FALSE(x.empty());
			ASSERT_EQ(x.size(), 1u);
			ASSERT_GE(x.bucket_count(), 1u);
			ASSERT_TRUE(x.contains(1));

			map_type y{allocator_type(alloc_id)};
			y = x;
			ASSERT_EQ(y.get_allocator(), x.get_allocator());
			EXPECT_FALSE(y.empty());
			EXPECT_EQ(y.size(), 1u);
			EXPECT_EQ(y.bucket_count(), x.bucket_count());
			EXPECT_EQ(y.at(1), "one");
		}
		{
			constexpr int x_alloc_id = 123, y_alloc_id = 456;
			map_type x{7, allocator_type(x_alloc_id)};
			x.try_emplace(1, "one");
			ASSERT_FALSE(x.empty());
			ASSERT_EQ(x.size(), 1u);
			ASSERT_GE(x.bucket_count(), 1u);
			ASSERT_TRUE(x.contains(1));

			map_type y{allocator_type(y_alloc_id)};
			y = x;
			ASSERT_NE(y.get_allocator(), x.get_allocator());
			EXPECT_FALSE(y.empty());
			EXPECT_EQ(y.size(), 1u);
			EXPECT_EQ(y.bucket_count(), x.bucket_count());
			EXPECT_EQ(y.at(1), "one");
		}
		{
			constexpr int x_alloc_id = 123, y_alloc_id = 456;
			map_type x{7, allocator_type(x_alloc_id)};
			x.try_emplace(1, "one");
			ASSERT_FALSE(x.empty());
			ASSERT_EQ(x.size(), 1u);
			ASSERT_GE(x.bucket_count(), 1u);
			ASSERT_TRUE(x.contains(1));

			map_type y{7, allocator_type(y_alloc_id)};
			y = x;
			ASSERT_NE(y.get_allocator(), x.get_allocator());
			EXPECT_FALSE(y.empty());
			EXPECT_EQ(y.size(), 1u);
			EXPECT_EQ(y.bucket_count(), x.bucket_count());
			EXPECT_EQ(y.at(1), "one");
		}
	}
}
TEST_F(sh_openmap, operator_assign_move)
{
	counted_openmap<int, std::string> x(7);
	x.try_emplace(1, "one");
	ASSERT_FALSE(x.empty());
	ASSERT_EQ(x.size(), 1u);
	const auto x_bucket_count = x.bucket_count();
	ASSERT_GE(x_bucket_count, 1u);
	ASSERT_TRUE(x.contains(1));

	counted_openmap<int, std::string> y;
	y = std::move(x);
	EXPECT_FALSE(y.empty());
	EXPECT_EQ(y.size(), 1u);
	EXPECT_EQ(y.bucket_count(), x_bucket_count);
	EXPECT_EQ(y.at(1), "one");

	y = std::move(y);
}
TEST_F(sh_openmap, operator_assign_move_alloc)
{
	{
		constexpr bool propagate = true;
		using allocator_type = stateful_allocator<std::pair<int, std::string>, propagate>;
		using map_type = openmap<int, std::string, std::hash<int>, std::equal_to<int>, allocator_type>;
		{
			constexpr int alloc_id = 123;
			map_type x{7, allocator_type(alloc_id)};
			x.try_emplace(1, "one");
			ASSERT_FALSE(x.empty());
			ASSERT_EQ(x.size(), 1u);
			const auto x_bucket_count = x.bucket_count();
			ASSERT_GE(x_bucket_count, 1u);
			ASSERT_TRUE(x.contains(1));

			map_type y{allocator_type(alloc_id)};
			y = std::move(x);
			ASSERT_EQ(y.get_allocator(), x.get_allocator());
			EXPECT_FALSE(y.empty());
			EXPECT_EQ(y.size(), 1u);
			EXPECT_EQ(y.bucket_count(), x_bucket_count);
			EXPECT_EQ(y.at(1), "one");
		}
		{
			constexpr int x_alloc_id = 123, y_alloc_id = 456;
			map_type x{7, allocator_type(x_alloc_id)};
			x.try_emplace(1, "one");
			ASSERT_FALSE(x.empty());
			ASSERT_EQ(x.size(), 1u);
			const auto x_bucket_count = x.bucket_count();
			ASSERT_GE(x_bucket_count, 1u);
			ASSERT_TRUE(x.contains(1));

			map_type y{allocator_type(y_alloc_id)};
			y = std::move(x);
			ASSERT_EQ(y.get_allocator(), x.get_allocator());
			EXPECT_FALSE(y.empty());
			EXPECT_EQ(y.size(), 1u);
			EXPECT_EQ(y.bucket_count(), x_bucket_count);
			EXPECT_EQ(y.at(1), "one");
		}
		{
			constexpr int alloc_id = 123;
			map_type x{7, allocator_type(alloc_id)};
			x.try_emplace(1, "one");
			ASSERT_FALSE(x.empty());
			ASSERT_EQ(x.size(), 1u);
			const auto x_bucket_count = x.bucket_count();
			ASSERT_GE(x_bucket_count, 1u);
			ASSERT_TRUE(x.contains(1));

			map_type y{7, allocator_type(alloc_id)};
			y = std::move(x);
			ASSERT_EQ(y.get_allocator(), x.get_allocator());
			EXPECT_FALSE(y.empty());
			EXPECT_EQ(y.size(), 1u);
			EXPECT_EQ(y.bucket_count(), x_bucket_count);
			EXPECT_EQ(y.at(1), "one");
		}
	}
	{
		constexpr bool propagate = false;
		using allocator_type = stateful_allocator<std::pair<int, std::string>, propagate>;
		using map_type = openmap<int, std::string, std::hash<int>, std::equal_to<int>, allocator_type>;
		{
			constexpr int alloc_id = 123;
			map_type x{7, allocator_type(alloc_id)};
			x.try_emplace(1, "one");
			ASSERT_FALSE(x.empty());
			ASSERT_EQ(x.size(), 1u);
			const auto x_bucket_count = x.bucket_count();
			ASSERT_GE(x_bucket_count, 1u);
			ASSERT_TRUE(x.contains(1));

			map_type y{allocator_type(alloc_id)};
			y = std::move(x);
			ASSERT_EQ(y.get_allocator(), x.get_allocator());
			EXPECT_FALSE(y.empty());
			EXPECT_EQ(y.size(), 1u);
			EXPECT_EQ(y.bucket_count(), x_bucket_count);
			EXPECT_EQ(y.at(1), "one");
		}
		{
			constexpr int x_alloc_id = 123, y_alloc_id = 456;
			map_type x{7, allocator_type(x_alloc_id)};
			x.try_emplace(1, "one");
			ASSERT_FALSE(x.empty());
			ASSERT_EQ(x.size(), 1u);
			const auto x_bucket_count = x.bucket_count();
			ASSERT_GE(x_bucket_count, 1u);
			ASSERT_TRUE(x.contains(1));

			map_type y{allocator_type(y_alloc_id)};
			y = std::move(x);
			ASSERT_NE(y.get_allocator(), x.get_allocator());
			EXPECT_FALSE(y.empty());
			EXPECT_EQ(y.size(), 1u);
			EXPECT_EQ(y.bucket_count(), x_bucket_count);
			EXPECT_EQ(y.at(1), "one");
		}
		{
			constexpr int x_alloc_id = 123, y_alloc_id = 456;
			map_type x{7, allocator_type(x_alloc_id)};
			x.try_emplace(1, "one");
			ASSERT_FALSE(x.empty());
			ASSERT_EQ(x.size(), 1u);
			const auto x_bucket_count = x.bucket_count();
			ASSERT_GE(x_bucket_count, 1u);
			ASSERT_TRUE(x.contains(1));

			map_type y{7, allocator_type(y_alloc_id)};
			y = std::move(x);
			ASSERT_NE(y.get_allocator(), x.get_allocator());
			EXPECT_FALSE(y.empty());
			EXPECT_EQ(y.size(), 1u);
			EXPECT_EQ(y.bucket_count(), x_bucket_count);
			EXPECT_EQ(y.at(1), "one");
		}
	}
}
TEST_F(sh_openmap, bucket)
{
	counted_openmap<int, std::string> x(2);
	x.try_emplace(1, "one");
	x.try_emplace(2, "two");
	x.try_emplace(3, "three");
	x.try_emplace(4, "four");
	ASSERT_FALSE(x.empty());
	ASSERT_EQ(x.size(), 4u);

	const auto& hasher = x.hash_function();
	for (int i = 1; i <= 4; ++i)
	{
		const auto index = hash_clamp(hasher, i, x.bucket_count());
		EXPECT_EQ(x.bucket(i), index);
	}
}
TEST_F(sh_openmap, bucket_size)
{
	struct direct_int_hash
	{
		std::size_t operator()(const int value) const
		{
			return value;
		}
	};
	openmap<int, std::string, direct_int_hash> x(8);
	x.try_emplace(1, "one");
	x.try_emplace(2, "two");
	x.try_emplace(3, "three");
	x.try_emplace(4, "four");
	ASSERT_FALSE(x.empty());
	ASSERT_EQ(x.size(), 4u);

	std::vector<bool> filled(x.bucket_count(), false);

	// This will fail if there are any collisions for [1,4] in a openmap of x.bucket_count(),
	// which is not strictly valid, but a reasonable sanity check.
	const auto& hasher = x.hash_function();
	for (int i = 1; i <= 4; ++i)
	{
		const auto index = hash_clamp(hasher, i, x.bucket_count());
		EXPECT_EQ(x.bucket_size(decltype(x.size())(index)), 1u);
		filled[index] = true;
	}

	// Check that empty buckets are in fact empty:
	for (std::size_t i = 0; i < filled.size(); ++i)
	{
		if (!filled[i])
		{
			EXPECT_EQ(x.bucket_size(decltype(x.size())(i)), 0u);
		}
	}
}
TEST_F(sh_openmap, load_factor)
{
	counted_openmap<int, std::string> x(8);
	ASSERT_FLOAT_EQ(x.load_factor(), 0.0f);

	x.try_emplace(1, "one");
	ASSERT_FLOAT_EQ(x.load_factor(), float(x.size()) / x.bucket_count());
	x.try_emplace(2, "two");
	ASSERT_FLOAT_EQ(x.load_factor(), float(x.size()) / x.bucket_count());
	x.try_emplace(3, "three");
	ASSERT_FLOAT_EQ(x.load_factor(), float(x.size()) / x.bucket_count());
}
TEST_F(sh_openmap, max_load_factor)
{
	counted_openmap<int, std::string> x(8);
	constexpr float max_load_factor = 0.25f;
	x.max_load_factor(max_load_factor);
	ASSERT_FLOAT_EQ(x.max_load_factor(), max_load_factor);
	ASSERT_FLOAT_EQ(x.load_factor(), 0.0f);

	const char* const values[] = { "zero", "one", "two", "three", "four", "five", "six", "seven" };
	for (int i = 0; i < int{ sizeof(values) / sizeof(values[0]) }; ++i)
	{
		x.try_emplace(i, values[i]);
		ASSERT_LE(x.load_factor(), max_load_factor);
	}
}
TEST_F(sh_openmap, key_eq)
{
	counted_openmap<int, std::string> x(8);
	EXPECT_TRUE(x.key_eq()(1, 1));
	EXPECT_FALSE(x.key_eq()(1, 2));
}
TEST_F(sh_openmap, begin_end)
{
	counted_openmap<int, std::string> x = {
		{ 1, "one" },
		{ 2, "two" },
		{ 3, "three" },
		{ 4, "four" },
	};

	int key_sum = 0;
	int value_sum = 0;
	for (auto it = x.begin(); it != x.end(); ++it)
	{
		static_assert(std::is_const_v<std::remove_reference_t<decltype(it->first)>>, "iterator key expected to be read-only");
		static_assert(false == std::is_const_v<std::remove_reference_t<decltype(it->second)>>, "iterator value expected to be mutable");
		static_assert(false == std::is_const_v<std::remove_reference_t<decltype(*it)>>, "dereferenced iterator value expected to be mutable");
		key_sum += it->first;
		for (const char c : it->second)
		{
			value_sum += c - 'a';
		}
	}
	EXPECT_EQ(key_sum, 10);
	EXPECT_EQ(value_sum, 193);
}
TEST_F(sh_openmap, cbegin_cend)
{
	const counted_openmap<int, std::string> x = {
		{ 1, "one" },
		{ 2, "two" },
		{ 3, "three" },
		{ 4, "four" },
	};

	int key_sum = 0;
	int value_sum = 0;
	for (auto it = x.cbegin(); it != x.cend(); ++it)
	{
		static_assert(std::is_const_v<std::remove_reference_t<decltype(*it)>>, "dereferenced const_iterator value expected to be read-only");
		key_sum += it->first;
		for (const char c : it->second)
		{
			value_sum += c - 'a';
		}
	}
	EXPECT_EQ(key_sum, 10);
	EXPECT_EQ(value_sum, 193);
}
TEST_F(sh_openmap, clear)
{
	counted_openmap<int, std::string> x = {
		{ 1, "one" },
		{ 2, "two" },
		{ 3, "three" },
		{ 4, "four" },
	};
	ASSERT_FALSE(x.empty());
	for (int i = 1; i <= 4; ++i)
	{
		ASSERT_TRUE(x.contains(i));
	}
	ASSERT_EQ(x.size(), 4u);

	x.clear();
	EXPECT_TRUE(x.empty());
	for (int i = 1; i <= 4; ++i)
	{
		EXPECT_FALSE(x.contains(i));
	}
	EXPECT_EQ(x.size(), 0u);
}
TEST_F(sh_openmap, swap)
{
	counted_openmap<int, std::string> y, x = {
		{ 1, "one" },
		{ 2, "two" },
		{ 3, "three" },
		{ 4, "four" },
	};
	ASSERT_FALSE(x.empty());
	ASSERT_EQ(x.size(), 4u);
	ASSERT_TRUE(y.empty());
	ASSERT_EQ(y.size(), 0u);

	x.swap(y);
	EXPECT_TRUE(x.empty());
	EXPECT_EQ(x.size(), 0u);
	EXPECT_FALSE(y.empty());
	EXPECT_EQ(y.size(), 4u);

	y.swap(y);
	EXPECT_FALSE(y.empty());
	EXPECT_EQ(y.size(), 4u);
}
TEST_F(sh_openmap, swap_iterator)
{
	counted_openmap<int, std::string> y, x = { { 1, "one" } };
	ASSERT_FALSE(x.empty());
	ASSERT_EQ(x.size(), 1u);
	EXPECT_TRUE(y.empty());
	EXPECT_EQ(y.size(), 0u);

	const auto it = x.begin();
	ASSERT_NE(it, x.end());
	ASSERT_EQ(it->first, 1u);
	ASSERT_EQ(it->second, "one");

	x.swap(y);
	EXPECT_TRUE(x.empty());
	EXPECT_EQ(x.size(), 0u);
	EXPECT_FALSE(y.empty());
	EXPECT_EQ(y.size(), 1u);

	EXPECT_EQ(it, y.begin());
	ASSERT_NE(it, y.end());
	EXPECT_EQ(it->first, 1u);
	EXPECT_EQ(it->second, "one");
}
TEST_F(sh_openmap, swap_alloc)
{
	{
		const std::initializer_list<std::pair<int, std::string>> l = {
			{ 1, "one" },
			{ 2, "two" },
			{ 3, "three" },
			{ 4, "four" },
		};
		{
			constexpr bool propagate = true;
			using allocator_type = stateful_allocator<std::pair<int, std::string>, propagate>;
			using map_type = openmap<int, std::string, std::hash<int>, std::equal_to<int>, allocator_type>;
			{
				constexpr int alloc_id = 123;
				map_type x{l, 8, allocator_type(alloc_id)};
				map_type y{allocator_type(alloc_id)};
				ASSERT_FALSE(x.empty());
				ASSERT_EQ(x.size(), 4u);
				ASSERT_TRUE(y.empty());
				ASSERT_EQ(y.size(), 0u);

				x.swap(y);
				EXPECT_EQ(x.get_allocator(), y.get_allocator());
				EXPECT_TRUE(x.empty());
				EXPECT_EQ(x.size(), 0u);
				EXPECT_FALSE(y.empty());
				EXPECT_EQ(y.size(), 4u);
			}
			{
				constexpr int x_alloc_id = 123, y_alloc_id = 456;
				map_type x{l, 8, allocator_type(x_alloc_id)};
				map_type y{allocator_type(y_alloc_id)};
				ASSERT_FALSE(x.empty());
				ASSERT_EQ(x.size(), 4u);
				ASSERT_TRUE(y.empty());
				ASSERT_EQ(y.size(), 0u);

				x.swap(y);
				EXPECT_EQ(x.get_allocator().m_id, y_alloc_id);
				EXPECT_EQ(y.get_allocator().m_id, x_alloc_id);
				EXPECT_TRUE(x.empty());
				EXPECT_EQ(x.size(), 0u);
				EXPECT_FALSE(y.empty());
				EXPECT_EQ(y.size(), 4u);
			}
		}
		{
			constexpr bool propagate = false;
			using allocator_type = stateful_allocator<std::pair<int, std::string>, propagate>;
			using map_type = openmap<int, std::string, std::hash<int>, std::equal_to<int>, allocator_type>;
			{
				constexpr int alloc_id = 123;
				map_type x{l, 8, allocator_type(alloc_id)};
				map_type y{allocator_type(alloc_id)};
				ASSERT_FALSE(x.empty());
				ASSERT_EQ(x.size(), 4u);
				ASSERT_TRUE(y.empty());
				ASSERT_EQ(y.size(), 0u);

				x.swap(y);
				EXPECT_EQ(x.get_allocator(), y.get_allocator());
				EXPECT_TRUE(x.empty());
				EXPECT_EQ(x.size(), 0u);
				EXPECT_FALSE(y.empty());
				EXPECT_EQ(y.size(), 4u);
			}
			{
				constexpr int x_alloc_id = 123, y_alloc_id = 456;
				map_type x{l, 8, allocator_type(x_alloc_id)};
				map_type y{allocator_type(y_alloc_id)};
				ASSERT_FALSE(x.empty());
				ASSERT_EQ(x.size(), 4u);
				ASSERT_TRUE(y.empty());
				ASSERT_EQ(y.size(), 0u);

				x.swap(y);
				EXPECT_EQ(x.get_allocator().m_id, x_alloc_id);
				EXPECT_EQ(y.get_allocator().m_id, y_alloc_id);
				EXPECT_TRUE(x.empty());
				EXPECT_EQ(x.size(), 0u);
				EXPECT_FALSE(y.empty());
				EXPECT_EQ(y.size(), 4u);
			}
		}
	}
	{
		const std::initializer_list<std::pair<int, int>> l = {
			{ 1, 12 },
			{ 2, 34 },
			{ 3, 56 },
			{ 4, 78 },
		};
		{
			constexpr bool propagate = false;
			using allocator_type = stateful_allocator<std::pair<int, int>, propagate>;
			using map_type = openmap<int, int, std::hash<int>, std::equal_to<int>, allocator_type>;
			{
				constexpr int x_alloc_id = 123, y_alloc_id = 456;
				map_type x{l, 8, allocator_type(x_alloc_id)};
				map_type y{allocator_type(y_alloc_id)};
				ASSERT_FALSE(x.empty());
				ASSERT_EQ(x.size(), 4u);
				ASSERT_TRUE(y.empty());
				ASSERT_EQ(y.size(), 0u);

				x.swap(y);
				EXPECT_EQ(x.get_allocator().m_id, x_alloc_id);
				EXPECT_EQ(y.get_allocator().m_id, y_alloc_id);
				EXPECT_TRUE(x.empty());
				EXPECT_EQ(x.size(), 0u);
				EXPECT_FALSE(y.empty());
				EXPECT_EQ(y.size(), 4u);
			}
		}
	}
}
TEST_F(sh_openmap, reserve)
{
	counted_openmap<int, std::string> x(16);
	x.max_load_factor(1.0);
	ASSERT_EQ(x.bucket_count(), 16u);

	x.reserve(32);
	ASSERT_EQ(x.bucket_count(), 32u);
}
TEST_F(sh_openmap, rehash)
{
	counted_openmap<int, std::string> x(32);
	x.max_load_factor(1.0f);
	ASSERT_EQ(x.bucket_count(), 32u);

	x.insert({ { 1, "one" }, { 2, "two" }, { 3, "three" }, { 4, "four" } });
	ASSERT_EQ(x.size(), 4u);
	for (int i = 1; i <= 4; ++i)
	{
		ASSERT_TRUE(x.contains(i));
	}

	x.rehash(64);
	ASSERT_EQ(x.bucket_count(), 64u);
	ASSERT_EQ(x.size(), 4u);
	for (int i = 1; i <= 4; ++i)
	{
		ASSERT_TRUE(x.contains(i));
	}

	x.rehash(16);
	ASSERT_EQ(x.bucket_count(), 16u);
	ASSERT_EQ(x.size(), 4u);
	for (int i = 1; i <= 4; ++i)
	{
		ASSERT_TRUE(x.contains(i));
	}

	x.rehash(1);
	ASSERT_EQ(x.bucket_count(), 4u);
	ASSERT_EQ(x.size(), 4u);
	for (int i = 1; i <= 4; ++i)
	{
		ASSERT_TRUE(x.contains(i));
	}
}
TEST_F(sh_openmap, insert)
{
	counted_openmap<int, std::string> x(4);
	{
		const std::pair<int, std::string> value(1, "one");
		const auto it = x.insert(value);
		EXPECT_TRUE(it.second);
		EXPECT_EQ(it.first->first, 1);
		EXPECT_EQ(it.first->second, "one");
	}
	{
		const std::pair<int, std::string> value(1, "one!");
		const auto it = x.insert(value);
		EXPECT_FALSE(it.second);
		EXPECT_EQ(it.first->first, 1);
		EXPECT_EQ(it.first->second, "one");
	}
	{
		std::pair<int, std::string> value(2, "two");
		const auto it = x.insert(std::move(value));
		EXPECT_TRUE(it.second);
		EXPECT_EQ(it.first->first, 2);
		EXPECT_EQ(it.first->second, "two");
	}
	{
		std::pair<int, std::string> value(2, "two!");
		const auto it = x.insert(std::move(value));
		EXPECT_FALSE(it.second);
		EXPECT_EQ(it.first->first, 2);
		EXPECT_EQ(it.first->second, "two");
	}
	ASSERT_EQ(x.size(), 2u);
	{
		const std::vector<std::pair<int, std::string>> values = {
			{ 1, "one" },
			{ 2, "two" },
			{ 3, "three" },
		};
		x.insert(values.begin(), values.end());
		EXPECT_TRUE(x.contains(3));
		EXPECT_EQ(x.at(3), "three");
	}
	ASSERT_EQ(x.size(), 3u);
	{
		x.insert({
			{ 1, "one" },
			{ 2, "two" },
			{ 3, "three" },
			{ 4, "four" },
		});
		EXPECT_EQ(x.size(), 4u);
		EXPECT_TRUE(x.contains(4));
		EXPECT_EQ(x.at(4), "four");
	}

	const auto hint = x.insert({ 5, "five" });
	{
		const auto it = x.insert(hint.first, { 6, "six" });
		EXPECT_EQ(it->first, 6);
		EXPECT_EQ(it->second, "six");
	}
	{
		const auto it = x.insert(hint.first, { 6, "six!" });
		EXPECT_EQ(it->first, 6);
		EXPECT_EQ(it->second, "six");
	}
	EXPECT_EQ(x.size(), 6u);
}
TEST_F(sh_openmap, insert_or_assign)
{
	counted_openmap<int, std::string> x(4);
	{
		const auto it = x.insert_or_assign(1, "one");
		EXPECT_TRUE(it.second);
		EXPECT_EQ(it.first->first, 1);
		EXPECT_EQ(it.first->second, "one");
	}
	{
		const auto it = x.insert_or_assign(1, "one!");
		EXPECT_FALSE(it.second);
		EXPECT_EQ(it.first->first, 1);
		EXPECT_EQ(it.first->second, "one!");
	}

	const auto hint = x.insert_or_assign(2, "two");
	{
		const auto it = x.insert_or_assign(hint.first, 3, "three");
		EXPECT_EQ(it->first, 3);
		EXPECT_EQ(it->second, "three");
	}
	{
		const auto it = x.insert_or_assign(hint.first, 3, "three!");
		EXPECT_EQ(it->first, 3);
		EXPECT_EQ(it->second, "three!");
	}
	EXPECT_EQ(x.size(), 3u);
}
TEST_F(sh_openmap, emplace)
{
	counted_openmap<int, std::string> x(4);
	{
		const auto it = x.emplace(1, "one");
		EXPECT_TRUE(it.second);
		EXPECT_EQ(it.first->first, 1);
		EXPECT_EQ(it.first->second, "one");
	}
	{
		const auto it = x.emplace(1, "one!");
		EXPECT_FALSE(it.second);
		EXPECT_EQ(it.first->first, 1);
		EXPECT_EQ(it.first->second, "one");
	}
	ASSERT_EQ(x.size(), 1u);
}
TEST_F(sh_openmap, emplace_transparent)
{
	std::uint32_t hashed_normally = 0, hashed_transparently = 0;
	openmap<int, int, transparent_hash<int>, std::equal_to<>> x{
		2,
		transparent_hash<int>{ hashed_normally, hashed_transparently }
	};

	x.emplace(1, 100);
	EXPECT_GE(hashed_normally, 1u);
	EXPECT_EQ(hashed_transparently, 0u);

	x.emplace(static_cast<unsigned short>(2), 200);
	EXPECT_GE(hashed_transparently, 1u);
}
TEST_F(sh_openmap, emplace_hint)
{
	counted_openmap<int, std::string> x(4);

	const auto hint = x.try_emplace(1, "one");
	ASSERT_TRUE(hint.second);
	EXPECT_EQ(hint.first->first, 1);
	EXPECT_EQ(hint.first->second, "one");

	{
		const auto it = x.emplace_hint(hint.first, 2, "two");
		EXPECT_EQ(it->first, 2);
		EXPECT_EQ(it->second, "two");
	}
	ASSERT_EQ(x.size(), 2u);
}
TEST_F(sh_openmap, try_emplace)
{
	counted_openmap<int, std::string> x(4);
	{
		const auto it = x.try_emplace(1, "one");
		EXPECT_TRUE(it.second);
		EXPECT_EQ(it.first->first, 1);
		EXPECT_EQ(it.first->second, "one");
	}
	{
		const auto it = x.try_emplace(1, "one!");
		EXPECT_FALSE(it.second);
		EXPECT_EQ(it.first->first, 1);
		EXPECT_EQ(it.first->second, "one");
	}
	ASSERT_EQ(x.size(), 1u);
}
TEST_F(sh_openmap, try_emplace_transparent)
{
	std::uint32_t used_normally = 0, used_transparently = 0;
	openmap<int, int, transparent_hash<int>, std::equal_to<>> x(4, transparent_hash<int>{ used_normally, used_transparently });

	x.try_emplace(1, 100);
	EXPECT_GE(used_normally, 1u);
	EXPECT_EQ(used_transparently, 0u);

	x.try_emplace(static_cast<unsigned short>(2), 200);
	EXPECT_GE(used_transparently, 1u);
}
TEST_F(sh_openmap, try_emplace_hint)
{
	counted_openmap<int, std::string> x(4);

	const auto hint = x.try_emplace(1, "one");
	ASSERT_TRUE(hint.second);
	EXPECT_EQ(hint.first->first, 1);
	EXPECT_EQ(hint.first->second, "one");

	{
		const auto it = x.try_emplace(hint.first, 2, "two");
		EXPECT_EQ(it->first, 2);
		EXPECT_EQ(it->second, "two");
	}
	ASSERT_EQ(x.size(), 2u);
}
TEST_F(sh_openmap, try_emplace_hint_transparent)
{
	std::uint32_t used_normally = 0, used_transparently = 0;
	openmap<int, int, transparent_hash<int>, std::equal_to<>> x(4, transparent_hash<int>{ used_normally, used_transparently });

	x.try_emplace(x.end(), 1, 100);
	EXPECT_GE(used_normally, 1u);
	EXPECT_EQ(used_transparently, 0u);

	x.try_emplace(x.end(), static_cast<unsigned short>(2), 200);
	EXPECT_GE(used_transparently, 1u);
}
TEST_F(sh_openmap, erase)
{
	counted_openmap<int, std::string> x = {
		{ 1, "one" },
		{ 2, "two" },
		{ 3, "three" },
		{ 4, "four" },
	};
	ASSERT_FALSE(x.empty());
	ASSERT_EQ(x.size(), 4u);

	ASSERT_EQ(x.erase(0), 0u);

	ASSERT_EQ(x.erase(2), 1u);
	EXPECT_EQ(x.contains(2), false);
	ASSERT_EQ(x.find(2), x.end());
	ASSERT_EQ(x.size(), 3u);

	x.erase(x.find(3));
	EXPECT_EQ(x.contains(3), false);
	ASSERT_EQ(x.find(3), x.end());
	ASSERT_EQ(x.size(), 2u);

	x.erase(x.find(1));
	ASSERT_EQ(x.size(), 1u);
	x.erase(x.find(4));
	EXPECT_EQ(x.size(), 0u);
	EXPECT_TRUE(x.empty());
}
TEST_F(sh_openmap, erase_transparent)
{
	std::uint32_t used_normally = 0, used_transparently = 0;
	openmap<int, int, transparent_hash<int>, std::equal_to<>> x{
		{
			{ 1, 100 },
			{ 2, 200 },
		},
		2,
		transparent_hash<int>{ used_normally, used_transparently }
	};

	x.erase(1);
	EXPECT_GE(used_normally, 1u);
	EXPECT_EQ(used_transparently, 0u);

	x.erase(static_cast<unsigned short>(2));
	EXPECT_GE(used_transparently, 1u);
}
TEST_F(sh_openmap, erase_range)
{
	openmap<std::size_t, std::size_t> x;

	constexpr std::size_t min = 47;
	constexpr std::size_t size = 149;
	for (std::size_t a = min; a < min + size; ++a)
	{
		x.try_emplace(a, a * 100);
	}
	ASSERT_FALSE(x.empty());
	ASSERT_EQ(x.size(), size);

	const auto first = std::next(x.begin());
	const auto front_key = x.begin()->first;
	const auto last = std::prev(x.end());
	const auto back_key = last->first;
	x.erase(first, last);

	EXPECT_FALSE(x.empty());
	EXPECT_EQ(x.size(), 2u);
	EXPECT_TRUE(x.contains(front_key));
	EXPECT_TRUE(x.contains(back_key));

	x.erase(x.begin(), x.end());
	EXPECT_TRUE(x.empty());
	EXPECT_EQ(x.size(), 0u);
	EXPECT_FALSE(x.contains(front_key));
	EXPECT_FALSE(x.contains(back_key));
}
TEST_F(sh_openmap, erase_continue)
{
	counted_openmap<int, std::string> x = {
		{ 1, "one" },
		{ 2, "two" },
		{ 3, "three" },
		{ 4, "four" },
	};
	ASSERT_FALSE(x.empty());
	ASSERT_EQ(x.size(), 4u);

	std::ptrdiff_t sum = 0;
	std::size_t count = 0;

	auto it = std::next(x.begin());
	ASSERT_NE(it, x.end());
	while (it != x.end())
	{
		sum += it->first;
		++count;
		it = x.erase_continue(it);

		ASSERT_EQ(x.size(), 4u - count);
	}
	ASSERT_EQ(count, 3u);
	ASSERT_EQ(x.size(), 1u);
	ASSERT_FALSE(x.empty());

	it = x.begin();
	sum += it->first;
	++count;
	ASSERT_EQ(x.erase_continue(it), x.end());

	EXPECT_EQ(sum, 10);
	EXPECT_EQ(count, 4u);
	EXPECT_EQ(x.size(), 0u);
	EXPECT_TRUE(x.empty());
}
TEST_F(sh_openmap, erase_continue_range)
{
	openmap<std::size_t, std::size_t> x;

	constexpr std::size_t min = 47;
	constexpr std::size_t size = 149;
	for (std::size_t a = min; a < min + size; ++a)
	{
		x.try_emplace(a, a * 100);
	}
	ASSERT_FALSE(x.empty());
	ASSERT_EQ(x.size(), size);

	const auto first = std::next(x.begin());
	const auto front_key = x.begin()->first;
	const auto last = std::prev(x.end());
	const auto back_key = last->first;
	auto result = x.erase_continue(first, last);

	ASSERT_FALSE(x.empty());
	EXPECT_EQ(result, std::prev(x.end()));
	EXPECT_EQ(x.size(), 2u);
	EXPECT_TRUE(x.contains(front_key));
	EXPECT_TRUE(x.contains(back_key));

	result = x.erase_continue(x.begin(), x.end());
	EXPECT_TRUE(x.empty());
	EXPECT_EQ(result, x.end());
	EXPECT_EQ(x.size(), 0u);
	EXPECT_FALSE(x.contains(front_key));
	EXPECT_FALSE(x.contains(back_key));
}
TEST_F(sh_openmap, operator_index)
{
	counted_openmap<int, std::string> x;
	ASSERT_TRUE(x.empty());
	ASSERT_EQ(x.size(), 0u);

	x[1];
	ASSERT_EQ(x.size(), 1u);
	EXPECT_TRUE(x.contains(1));
	EXPECT_EQ(x.at(1), "");

	x[1] = "one";
	ASSERT_EQ(x.size(), 1u);
	EXPECT_TRUE(x.contains(1));
	EXPECT_EQ(x.at(1), "one");
}
TEST_F(sh_openmap, operator_index_transparent)
{
	std::uint32_t used_normally = 0, used_transparently = 0;
	openmap<int, int, transparent_hash<int>, std::equal_to<>> x{
		{
			{ 1, 100 },
			{ 2, 200 },
		},
		2,
		transparent_hash<int>{ used_normally, used_transparently }
	};

	x[1];
	EXPECT_GE(used_normally, 1u);
	EXPECT_EQ(used_transparently, 0u);

	x[static_cast<unsigned short>(2)];
	EXPECT_GE(used_transparently, 1u);
}
TEST_F(sh_openmap, at)
{
	counted_openmap<int, std::string> x = {
		{ 1, "one" },
		{ 2, "two" },
	};
	ASSERT_FALSE(x.empty());
	ASSERT_EQ(x.size(), 2u);

	EXPECT_EQ(x.at(1), "one");
	EXPECT_THROW(x.at(0), std::out_of_range);
}
TEST_F(sh_openmap, at_transparent)
{
	std::uint32_t used_normally = 0, used_transparently = 0;
	openmap<int, int, transparent_hash<int>, std::equal_to<>> x{
		{
			{ 1, 100 },
			{ 2, 200 },
		},
		2,
		transparent_hash<int>{ used_normally, used_transparently }
	};

	x.at(1);
	EXPECT_GE(used_normally, 1u);
	EXPECT_EQ(used_transparently, 0u);

	x.at(static_cast<unsigned short>(2));
	EXPECT_GE(used_transparently, 1u);
}
TEST_F(sh_openmap, count)
{
	counted_openmap<int, std::string> x = {
		{ 1, "one" },
		{ 2, "two" },
		{ 3, "three" },
		{ 4, "four" },
	};
	ASSERT_FALSE(x.empty());
	ASSERT_EQ(x.size(), 4u);

	EXPECT_EQ(x.count(0), 0u);
	for (int i = 1; i <= 4; ++i)
	{
		EXPECT_EQ(x.count(i), 1u);
	}
}
TEST_F(sh_openmap, count_transparent)
{
	std::uint32_t used_normally = 0, used_transparently = 0;
	openmap<int, int, transparent_hash<int>, std::equal_to<>> x{
		{
			{ 1, 100 },
			{ 2, 200 },
		},
		2,
		transparent_hash<int>{ used_normally, used_transparently }
	};

	x.count(1);
	EXPECT_GE(used_normally, 1u);
	EXPECT_EQ(used_transparently, 0u);

	x.count(static_cast<unsigned short>(2));
	EXPECT_GE(used_transparently, 1u);
}
TEST_F(sh_openmap, contains)
{
	counted_openmap<int, std::string> x = {
		{ 1, "one" },
		{ 2, "two" },
		{ 3, "three" },
		{ 4, "four" },
	};
	ASSERT_FALSE(x.empty());
	ASSERT_EQ(x.size(), 4u);

	for (int i = 1; i <= 4; ++i)
	{
		EXPECT_TRUE(x.contains(i));
	}
}
TEST_F(sh_openmap, contains_transparent)
{
	std::uint32_t used_normally = 0, used_transparently = 0;
	openmap<int, int, transparent_hash<int>, std::equal_to<>> x{
		{
			{ 1, 100 },
			{ 2, 200 },
		},
		2,
		transparent_hash<int>{ used_normally, used_transparently }
	};

	x.contains(1);
	EXPECT_GE(used_normally, 1u);
	EXPECT_EQ(used_transparently, 0u);

	x.contains(static_cast<unsigned short>(2));
	EXPECT_GE(used_transparently, 1u);
}
TEST_F(sh_openmap, find)
{
	counted_openmap<int, std::string> x = {
		{ 1, "one" },
		{ 2, "two" },
		{ 3, "three" },
		{ 4, "four" },
	};
	ASSERT_FALSE(x.empty());
	ASSERT_EQ(x.size(), 4u);

	EXPECT_EQ(x.find(0), x.end());
	for (int i = 1; i <= 4; ++i)
	{
		EXPECT_NE(x.find(i), x.end());
	}
}
TEST_F(sh_openmap, find_transparent)
{
	std::uint32_t used_normally = 0, used_transparently = 0;
	counted_openmap<int, int, transparent_hash<int>, std::equal_to<>> x{
		{
			{ 1, 100 },
			{ 2, 200 },
		},
		2,
		transparent_hash<int>{ used_normally, used_transparently }
	};

	x.find(1);
	EXPECT_GE(used_normally, 1u);
	EXPECT_EQ(used_transparently, 0u);

	x.find(static_cast<unsigned short>(2));
	EXPECT_GE(used_transparently, 1u);
}
TEST_F(sh_openmap, equal_range)
{
	counted_openmap<int, std::string> x = {
		{ 1, "one" },
		{ 2, "two" },
		{ 3, "three" },
		{ 4, "four" },
	};
	ASSERT_FALSE(x.empty());
	ASSERT_EQ(x.size(), 4u);

	{
		const auto range = x.equal_range(0);
		EXPECT_EQ(range.first, range.second);
		EXPECT_EQ(range.first, x.end());
		EXPECT_EQ(range.second, x.end());
	}
	{
		const auto range = x.equal_range(2);
		EXPECT_NE(range.first, range.second);
		EXPECT_NE(range.first, x.end());
		ASSERT_NE(range.second, x.end());
		EXPECT_EQ(range.first->first, 2);
		EXPECT_EQ(range.first->second, "two");
	}
}
TEST_F(sh_openmap, ctor_move_use)
{
	counted_openmap<int, std::string> x(2);
	x.try_emplace(0, "zero");
	x.try_emplace(1, "one");
	ASSERT_FALSE(x.empty());
	ASSERT_EQ(x.size(), 2u);

	counted_openmap<int, std::string> y(std::move(x));
	EXPECT_FALSE(y.empty());
	EXPECT_EQ(y.size(), 2u);
	EXPECT_EQ(y.at(0), "zero");
	EXPECT_EQ(y.at(1), "one");

	ASSERT_EQ(x.size(), 0u);
	ASSERT_FALSE(x.contains(0));
	ASSERT_FALSE(x.contains(1));
	x.try_emplace(2, "two");
	x.try_emplace(3, "three");
	EXPECT_EQ(x.size(), 2u);
	EXPECT_EQ(x.at(2), "two");
	EXPECT_EQ(x.at(3), "three");
}
TEST_F(sh_openmap, narrow)
{
	counted_openmap<int, int> x(1024);

	for (int i = 0; i < 512; ++i)
	{
		const auto it_emplaced = x.try_emplace(i, i);
		ASSERT_TRUE(it_emplaced.second);
		EXPECT_EQ(it_emplaced.first->first, i);
		EXPECT_EQ(it_emplaced.first->second, i);
	}
	ASSERT_EQ(x.size(), 512u);
	for (int i = 0; i < 512; ++i)
	{
		const auto it = x.find(i);
		ASSERT_NE(it, x.end());
		EXPECT_EQ(it->first, i);
		EXPECT_EQ(it->second, i);
	}
}
TEST_F(sh_openmap, widen)
{
	counted_openmap<int, int, thousands_hash<int>> x(1024);

	for (int i = 0; i < 512; ++i)
	{
		const auto it_emplaced = x.try_emplace(i, i);
		ASSERT_TRUE(it_emplaced.second);
		EXPECT_EQ(it_emplaced.first->first, i);
		EXPECT_EQ(it_emplaced.first->second, i);
	}
	ASSERT_EQ(x.size(), 512u);
	for (int i = 0; i < 512; ++i)
	{
		const auto it = x.find(i);
		ASSERT_NE(it, x.end());
		EXPECT_EQ(it->first, i);
		EXPECT_EQ(it->second, i);
	}
}
TEST_F(sh_openmap, widen_throw)
{
	bool can_allocate = true;
	openmap<int, int, thousands_hash<int>, std::equal_to<int>, unreliable_allocator<std::pair<int, int>>> x(1024, unreliable_allocator<std::pair<int, int>>(can_allocate));

	const auto emplace = [&x](const int i)
	{
		const auto it_emplaced = x.try_emplace(i, i);
		ASSERT_TRUE(it_emplaced.second);
		EXPECT_EQ(it_emplaced.first->first, i);
		EXPECT_EQ(it_emplaced.first->second, i);
	};

	// buckets 0...
	for (int i = 0; i <= 254; ++i)
	{
		emplace(i);
	}
	ASSERT_FALSE(x.empty());
	ASSERT_EQ(x.size(), 255u);

	can_allocate = false;

	EXPECT_THROW(emplace(255), std::bad_alloc);
	ASSERT_FALSE(x.empty());
	ASSERT_EQ(x.size(), 255u);
	ASSERT_FALSE(x.contains(255));
	for (int i = 0; i <= 254; ++i)
	{
		ASSERT_TRUE(x.contains(i));
	}

	emplace(987000); // bucket 1023
	EXPECT_THROW(emplace(987001), std::bad_alloc); // bucket 0 by wrapping
	ASSERT_FALSE(x.contains(987001));
	ASSERT_FALSE(x.empty());
	ASSERT_EQ(x.size(), 256u);
	for (int i = 0; i <= 254; ++i)
	{
		ASSERT_TRUE(x.contains(i));
	}
	ASSERT_TRUE(x.contains(987000));

	x.clear();

	// buckets 1022, 1023, 0...
	for (int i = 574000; i <= 574254; ++i)
	{
		emplace(i);
	}
	EXPECT_THROW(emplace(574255), std::bad_alloc); // bucket 0 by wrapping

#if 0
	for (int i = 1000; i <= 10'000'000; i += 1000)
	{
		if (hash_clamp(x.hash_function(), i, x.bucket_count()) == 769)
		{
			std::cerr << "found: " << i << std::endl;
			break;
		}
	}
#endif
}
TEST_F(sh_openmap, widen_and_continue_emplace)
{
	counted_openmap<int, int, thousands_hash<int>> x(1024);

	// Starts inserting in bucket 632.
	for (int i = 1000; i < 1254; ++i)
	{
		const auto it_emplaced = x.try_emplace(i, i);
		ASSERT_TRUE(it_emplaced.second);
		EXPECT_EQ(it_emplaced.first->first, i);
		EXPECT_EQ(it_emplaced.first->second, i);
	}
	ASSERT_EQ(x.size(), 254u);
	for (int i = 1000; i < 1254; ++i)
	{
		const auto it = x.find(i);
		ASSERT_NE(it, x.end());
		ASSERT_EQ(it->first, i);
		ASSERT_EQ(it->second, i);
	}
	// Starts inserting in bucket 611.
	for (int i = 22000; i < 22100; ++i)
	{
		const auto it_emplaced = x.try_emplace(i, i);
		ASSERT_TRUE(it_emplaced.second);
		EXPECT_EQ(it_emplaced.first->first, i);
		EXPECT_EQ(it_emplaced.first->second, i);
	}
	// Check everything.
	for (int i = 1000; i < 1254; ++i)
	{
		const auto it = x.find(i);
		ASSERT_NE(it, x.end());
		ASSERT_EQ(it->first, i);
		ASSERT_EQ(it->second, i);
	}
	for (int i = 22000; i < 22100; ++i)
	{
		const auto it = x.find(i);
		ASSERT_NE(it, x.end());
		ASSERT_EQ(it->first, i);
		ASSERT_EQ(it->second, i);
	}
	// Fill until rehash.
	for (int i = 0; i < 1000; ++i)
	{
		const auto it_emplaced = x.try_emplace(i, i);
		ASSERT_TRUE(it_emplaced.second);
		ASSERT_EQ(it_emplaced.first->first, i);
		ASSERT_EQ(it_emplaced.first->second, i);
	}
	// Check everything, again.
	for (int i = 0; i < 1254; ++i)
	{
		const auto it = x.find(i);
		ASSERT_NE(it, x.end());
		EXPECT_EQ(it->first, i);
		EXPECT_EQ(it->second, i);
	}
	for (int i = 22000; i < 22100; ++i)
	{
		const auto it = x.find(i);
		ASSERT_NE(it, x.end());
		EXPECT_EQ(it->first, i);
		EXPECT_EQ(it->second, i);
	}
}
TEST_F(sh_openmap, ctor_default_zero_allocation)
{
	general_allocations::get().reset();
	ASSERT_EQ(general_allocations::get().m_current, 0u);
	openmap<int, int, std::hash<int>, std::equal_to<int>, counted_allocator<int>> x;
	EXPECT_EQ(general_allocations::get().m_current, 0u);
}
TEST_F(sh_openmap, ctor_empty_zero_allocation)
{
	general_allocations::get().reset();
	ASSERT_EQ(general_allocations::get().m_current, 0u);
	openmap<int, int, std::hash<int>, std::equal_to<int>, counted_allocator<int>> x(0);
	EXPECT_EQ(general_allocations::get().m_current, 0u);
}
TEST_F(sh_openmap, ctor_dtor_test)
{
	class RefInt final
	{
	public:
		RefInt(const int value, int& refs, int& copies) noexcept
			: m_value(value)
			, m_refs(&refs)
			, m_copies(&copies)
		{
			++(*m_refs);
		}
		RefInt(const RefInt& other) noexcept
			: m_value(other.m_value)
			, m_refs(other.m_refs)
			, m_copies(other.m_copies)
		{
			if (m_refs != nullptr)
			{
				++(*m_refs);
			}
			assert(m_copies != nullptr);
			if (m_copies != nullptr)
			{
				++(*m_copies);
			}
		}
		RefInt(RefInt&& other) noexcept
			: m_value(other.m_value)
			, m_refs(other.m_refs)
		{
			if (m_refs != nullptr)
			{
				++(*m_refs);
			}
		}
		~RefInt()
		{
			if (m_refs != nullptr)
			{
				--(*m_refs);
			}
		}
		RefInt& operator=(const RefInt& other) noexcept
		{
			if (m_refs != nullptr)
			{
				--(*m_refs);
			}
			m_value = other.m_value;
			m_refs = other.m_refs;
			m_copies = other.m_copies;
			if (m_refs != nullptr)
			{
				++(*m_refs);
			}
			assert(m_copies != nullptr);
			if (m_copies != nullptr)
			{
				++(*m_copies);
			}
			return *this;
		}
		RefInt& operator=(RefInt&& other) noexcept
		{
			if (m_refs != nullptr)
			{
				--(*m_refs);
			}
			m_value = other.m_value;
			m_refs = other.m_refs;
			m_copies = other.m_copies;
			if (m_refs != nullptr)
			{
				++(*m_refs);
			}
			return *this;
		}

		constexpr bool operator==(const RefInt& other) const noexcept
		{
			return m_value == other.m_value;
		}
		constexpr operator int() const noexcept
		{
			return m_value;
		}

	private:
		int m_value;
		int* m_refs;
		int* m_copies;
	};
	struct Hash
	{
		using is_transparent = void;

		constexpr std::size_t operator()(const int& value) const noexcept
		{
			return std::size_t(value);
		}
		constexpr std::size_t operator()(const RefInt& value) const noexcept
		{
			return std::size_t(int(value));
		}
	};
	int key_copies = 0;
	int value_copies = 0;
	int key_references = 0;
	int value_references = 0;

	using map_type = openmap<RefInt, RefInt, Hash, std::equal_to<>, counted_allocator<std::pair<const RefInt, RefInt>>>;
	using value_type = sh::robinhood::mutable_key_value_pair<RefInt, RefInt>;
	using narrow_info_type = sh::robinhood::info<std::uint8_t>;

	general_allocations::get().reset();
	ASSERT_EQ(general_allocations::get().m_current, 0u);
	ASSERT_EQ(typed_allocations<value_type>::get().m_current, 0u);
	ASSERT_EQ(typed_allocations<narrow_info_type>::get().m_current, 0u);

	{
		// Sized as to force rehash numerous times.
		map_type hundreds(8);

		for (std::size_t rep = 0; rep < 2; ++rep)
		{
			for (int i = 0; i < 151; ++i)
			{
				// Test emplace.
				const bool emplaced = hundreds.try_emplace(RefInt { i * 100, key_references, key_copies }, RefInt { i, value_references, value_copies }).second;
				ASSERT_TRUE(emplaced);
				if constexpr (DEBUG_VERBOSE)
				{
					if (emplaced)
					{
						std::cerr << "Emplaced " << i << '\n';
						dump_openmap(hundreds);
					}
				}
			}
			for (int i = 50; i < 125; i += 3)
			{
				// Test erase.
				const bool erased = hundreds.erase(i * 100);
				ASSERT_TRUE(erased);
				if constexpr (DEBUG_VERBOSE)
				{
					if (erased)
					{
						std::cerr << "Erased " << i << '\n';
						dump_openmap(hundreds);
					}
				}
			}
			if (rep == 0)
			{
				// Test clear on the first iteration.
				hundreds.clear();
			}
		}
		// Test destruction while still populated after second iteration.
	}

	EXPECT_EQ(key_copies, 0);
	EXPECT_EQ(value_copies, 0);
	EXPECT_EQ(key_references, 0);
	EXPECT_EQ(value_references, 0);
	EXPECT_EQ(general_allocations::get().m_current, 0u);
	EXPECT_EQ(typed_allocations<value_type>::get().m_current, 0u);
	EXPECT_EQ(typed_allocations<narrow_info_type>::get().m_current, 0u);

	if constexpr (DEBUG_VERBOSE)
	{
		std::cerr << "general_allocations\n\t" << general_allocations::get() << '\n';
		std::cerr << "typed_allocations<value_type> (size " << sizeof(value_type) << ", align " << alignof(value_type) << ")\n\t"
			<< typed_allocations<value_type>::get() << '\n';
		std::cerr << "typed_allocations<narrow_info_type> (size " << sizeof(narrow_info_type) << ", align " << alignof(narrow_info_type) << ")\n\t"
			<< typed_allocations<narrow_info_type>::get() << '\n';
	}
}

