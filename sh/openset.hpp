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

#ifndef INC_SH__OPENSET_HPP
#define INC_SH__OPENSET_HPP

#include "robinhood.hpp"

#include <algorithm>
#include <cassert>
#include <functional>
#include <initializer_list>
#include <utility>

namespace sh
{

namespace robinhood
{
	/**	A mutable key container.
	 *	@note Provides a key member function to its contained value.
	 *	@tparam T The contained key value type.
	 */
	template <typename T>
	class mutable_key final
	{
	public:
		using value_type = T;

		/**	Cast to the internal value_type.
		 *	@return The internal value_type, m_value.
		 */
		constexpr explicit operator const value_type&() const noexcept
		{
			return m_value;
		}

		template <typename... Args>
		constexpr explicit mutable_key(Args&&... args)
			noexcept(std::is_nothrow_constructible_v<value_type, Args...>)
			: m_value(std::forward<Args>(args)...)
		{ }
		mutable_key() = default;
		mutable_key(const mutable_key&) = default;
		mutable_key(mutable_key&&) = default;
		~mutable_key() = default;
		mutable_key& operator=(const mutable_key&) = default;
		mutable_key& operator=(mutable_key&&) = default;

		/**	Provides mutable access to the contained key value.
		 *	@return A non-const reference to m_value.
		 */
		constexpr value_type& key() noexcept
		{
			return m_value;
		}
		/**	Provides const access to the contained key value.
		 *	@return A const reference to m_value.
		 */
		constexpr const value_type& key() const noexcept
		{
			return m_value;
		}

		/**	Swap keys with another mutable_key.
		 *	@param other The other mutable_key with which to swap keys.
		 */
		void swap(mutable_key& other)
			noexcept(std::is_nothrow_swappable_v<value_type>)
		{
			using std::swap;
			swap(key(), other.key());
		}
		/**	Swap the keys of two mutable_key objects.
		 *	@param lhs A mutable_key.
		 *	@param rhs Another mutable_key.
		 */
		friend void swap(mutable_key& lhs, mutable_key& rhs)
			noexcept(std::is_nothrow_swappable_v<value_type>)
		{
			lhs.swap(rhs);
		}

	private:
		/**	The contained key value.
		 */
		value_type m_value;
	};

	/**	A namespace-scoped is_trivial specialization for mutable_key.
	 *	@tparam T The contained key value type.
	 *	@details Will mark mutable_key as satisfying is trivial if its key type is trivially constructible, destructible, and copyable.
	 */
	template <typename T>
	struct is_trivial <mutable_key<T>> final
	{
		static constexpr bool value = std::is_trivially_constructible_v<T>
			&& std::is_trivially_destructible_v<T>
			&& std::is_trivially_copyable_v<T>;
	};

	/**	Used to implement a std::unordered_set-style, key-only Robinhood hashtable.
	 *	@tparam Key The type of key used in the set.
	 *	@tparam SizeType The size_type of the set buckets.
	 */
	template <typename Key, typename SizeType>
	using set_policy = robinhood::policy<Key, const Key, mutable_key<Key>, SizeType>;
} // namespace robinhood

template <typename Key,
	typename Hash = std::hash<Key>,
	typename KeyEqual = std::equal_to<Key>,
	typename Allocator = std::allocator<Key>,
	typename SizeType = robinhood::default_size_type>
class openset : private robinhood::hashtable<robinhood::set_policy<Key, SizeType>, Hash, KeyEqual, Allocator>
{
private:
	using hashtable_type = robinhood::hashtable<robinhood::set_policy<Key, SizeType>, Hash, KeyEqual, Allocator>;
	using typename hashtable_type::find_result;
	using typename hashtable_type::hash_result;
	using typename hashtable_type::wide_info_type;

public:
	using key_type = Key;
	using hasher = Hash;
	using key_equal = KeyEqual;
	using allocator_type = Allocator;
	using value_type = Key;
	using const_value_type = const value_type;
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

	openset() noexcept(std::is_nothrow_constructible_v<allocator_type>);
	explicit openset(const allocator_type& alloc) noexcept;
	explicit openset(size_type bucket_count, const hasher& hash = hasher(), const key_equal& equal = key_equal(), const allocator_type& alloc = allocator_type());
	openset(size_type bucket_count, const hasher& hash, const allocator_type& alloc);
	openset(size_type bucket_count, const allocator_type& alloc);
	openset(const openset& other);
	openset(const openset& other, const allocator_type& alloc);
	openset(openset&& other) noexcept(std::is_nothrow_constructible_v<allocator_type>);
	openset(openset&& other, const allocator_type& alloc) noexcept(typename hashtable_type::allocator_traits::is_always_equal());
	template <typename InputIt>
	openset(InputIt first, InputIt last, size_type bucket_count = robinhood::default_bucket_count, const hasher& hash = hasher(), const key_equal& equal = key_equal(), const allocator_type& alloc = allocator_type());
	template <typename InputIt>
	openset(InputIt first, InputIt last, size_type bucket_count, const hasher& hash, const allocator_type& alloc);
	template <typename InputIt>
	openset(InputIt first, InputIt last, size_type bucket_count, const allocator_type& alloc);
	openset(std::initializer_list<value_type> init, size_type bucket_count = robinhood::default_bucket_count, const hasher& hash = hasher(), const key_equal& equal = key_equal(), const allocator_type& alloc = allocator_type());
	openset(std::initializer_list<value_type> init, size_type bucket_count, const hasher& hash, const allocator_type& alloc);
	openset(std::initializer_list<value_type> init, size_type bucket_count, const allocator_type& alloc);
	~openset() = default;

	openset& operator=(const openset& other);
	openset& operator=(openset&& other) noexcept;
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
	void swap(openset& other) noexcept;

	inline std::pair<iterator, bool> insert(const value_type& value);
	inline std::pair<iterator, bool> insert(value_type&& value);
	inline iterator insert(const_iterator hint, const value_type& value);
	inline iterator insert(const_iterator hint, value_type&& value);
	template <typename InputIt>
	void insert(InputIt first, InputIt last);
	void insert(std::initializer_list<value_type> ilist);
	template <typename KeyArg>
	std::pair<iterator, bool> emplace(KeyArg&& key_arg);
	template <typename KeyArg>
	iterator emplace_hint(const_iterator hint, KeyArg&& key_arg);

	template <typename KeyArg>
	size_type erase(KeyArg&& key_arg);
	void erase(iterator pos);
	void erase(const_iterator pos);
	void erase(const_iterator first, const_iterator last);
	iterator erase_continue(const_iterator pos);
	iterator erase_continue(const_iterator first, const_iterator last);

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

template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
openset<Key, Hash, KeyEqual, Allocator, SizeType>::openset() noexcept(std::is_nothrow_constructible_v<allocator_type>)
	: hashtable_type(allocator_type())
{ }
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
openset<Key, Hash, KeyEqual, Allocator, SizeType>::openset(const allocator_type& alloc) noexcept
	: hashtable_type(alloc)
{ }
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
openset<Key, Hash, KeyEqual, Allocator, SizeType>::openset(const size_type bucket_count, const hasher& hash, const key_equal& equal, const allocator_type& alloc)
	: hashtable_type(bucket_count, hash, equal, alloc)
{ }
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
openset<Key, Hash, KeyEqual, Allocator, SizeType>::openset(const size_type bucket_count, const hasher& hash, const allocator_type& alloc)
	: hashtable_type(bucket_count, hash, key_equal(), alloc)
{ }
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
openset<Key, Hash, KeyEqual, Allocator, SizeType>::openset(const size_type bucket_count, const allocator_type& alloc)
	: hashtable_type(bucket_count, hasher(), key_equal(), alloc)
{ }
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
openset<Key, Hash, KeyEqual, Allocator, SizeType>::openset(const openset& other)
	: hashtable_type(other)
{ }
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
openset<Key, Hash, KeyEqual, Allocator, SizeType>::openset(const openset& other, const allocator_type& alloc)
	: hashtable_type(other, alloc)
{ }
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
openset<Key, Hash, KeyEqual, Allocator, SizeType>::openset(openset&& other) noexcept(std::is_nothrow_constructible_v<allocator_type>)
	: hashtable_type(std::move(other))
{ }
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
openset<Key, Hash, KeyEqual, Allocator, SizeType>::openset(openset&& other, const allocator_type& alloc) noexcept(typename hashtable_type::allocator_traits::is_always_equal())
	: hashtable_type(std::move(other), alloc)
{ }
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename InputIt>
openset<Key, Hash, KeyEqual, Allocator, SizeType>::openset(InputIt first, InputIt last, const size_type bucket_count, const hasher& hash, const key_equal& equal, const allocator_type& alloc)
	: hashtable_type(bucket_count, hash, equal, alloc)
{
	insert(first, last);
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename InputIt>
openset<Key, Hash, KeyEqual, Allocator, SizeType>::openset(InputIt first, InputIt last, const size_type bucket_count, const hasher& hash, const allocator_type& alloc)
	: hashtable_type(first, last, bucket_count, hash, key_equal(), alloc)
{ }
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename InputIt>
openset<Key, Hash, KeyEqual, Allocator, SizeType>::openset(InputIt first, InputIt last, const size_type bucket_count, const allocator_type& alloc)
	: openset(first, last, bucket_count, hasher(), key_equal(), alloc)
{ }
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
openset<Key, Hash, KeyEqual, Allocator, SizeType>::openset(std::initializer_list<value_type> init, const size_type bucket_count, const hasher& hash, const key_equal& equal, const allocator_type& alloc)
	: hashtable_type(bucket_count, hash, equal, alloc)
{
	insert(std::move(init));
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
openset<Key, Hash, KeyEqual, Allocator, SizeType>::openset(std::initializer_list<value_type> init, const size_type bucket_count, const hasher& hash, const allocator_type& alloc)
	: openset(std::move(init), bucket_count, hash, key_equal(), alloc)
{ }
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
openset<Key, Hash, KeyEqual, Allocator, SizeType>::openset(std::initializer_list<value_type> init, const size_type bucket_count, const allocator_type& alloc)
	: openset(std::move(init), bucket_count, hasher(), key_equal(), alloc)
{ }

template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::operator=(const openset& other) -> openset&
{
	this->hashtable_type::operator=(other);
	return *this;
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::operator=(openset&& other) noexcept -> openset&
{
	this->hashtable_type::operator=(std::move(other));
	return *this;
}

template <typename key, typename hash, typename keyequal, typename allocator, typename sizetype>
constexpr auto openset<key, hash, keyequal, allocator, sizetype>::begin() -> iterator
{
	return iterator(this->hashtable_type::begin());
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
constexpr auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::end() -> iterator
{
	return iterator(this->hashtable_type::end());
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
constexpr auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::begin() const -> const_iterator
{
	return cbegin();
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
constexpr auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::end() const -> const_iterator
{
	return cend();
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
constexpr auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::cbegin() const -> const_iterator
{
	return const_cast<openset&>(*this).begin();
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
constexpr auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::cend() const -> const_iterator
{
	return const_cast<openset&>(*this).end();
}

template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
constexpr auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::begin(const size_type n) -> local_iterator
{
	return local_iterator(this->hashtable_type::begin(n));
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
constexpr auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::end(const size_type n) -> local_iterator
{
	return local_iterator(this->hashtable_type::end(n));
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
constexpr auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::begin(const size_type n) const -> const_local_iterator
{
	return cbegin(n);
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
constexpr auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::end(const size_type n) const -> const_local_iterator
{
	return cend(n);
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
constexpr auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::cbegin(const size_type n) const -> const_local_iterator
{
	return const_cast<openset&>(*this).begin(n);
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
constexpr auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::cend(const size_type n) const -> const_local_iterator
{
	return const_cast<openset&>(*this).end(n);
}

template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
void openset<Key, Hash, KeyEqual, Allocator, SizeType>::swap(openset& other) noexcept
{
	this->hashtable_type::swap(other);
}

template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::insert(const value_type& value) -> std::pair<iterator, bool>
{
	return emplace(value);
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::insert(value_type&& value) -> std::pair<iterator, bool>
{
	return emplace(std::move(value));
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::insert(const_iterator hint, const value_type& value) -> iterator
{
	return emplace_hint(hint, std::move(value));
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::insert(const_iterator hint, value_type&& value) -> iterator
{
	return emplace_hint(hint, std::move(value));
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename InputIt>
void openset<Key, Hash, KeyEqual, Allocator, SizeType>::insert(InputIt first, InputIt last)
{
	while (first != last)
	{
		insert(*(first++));
	}
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
void openset<Key, Hash, KeyEqual, Allocator, SizeType>::insert(std::initializer_list<value_type> ilist)
{
	for (const value_type& value : ilist)
	{
		insert(value);
	}
}

template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename KeyArg>
auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::emplace(KeyArg&& key_arg) -> std::pair<iterator, bool>
{
	auto&& key = this->hashtable_type::do_key(std::forward<KeyArg>(key_arg));
	const hash_result hash = static_cast<const hasher&>(*this)(key);
	const find_result result = this->hashtable_type::do_find(hash, key);
	return std::make_pair(
		iterator(
			this->hashtable_type::index(
				result.m_found
					? result.m_index
					: this->hashtable_type::do_emplace(
						wide_info_type(result.m_distance, hash),
						result.m_index,
						std::forward<decltype(key)>(key)).m_index
			)
		),
		result.m_found == false);
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename KeyArg>
auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::emplace_hint(const const_iterator hint, KeyArg&& key_arg) -> iterator
{
	return emplace(std::forward<KeyArg>(key_arg)).first;
}

template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename KeyArg>
auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::erase(KeyArg&& key_arg) -> size_type
{
	const iterator it = find(std::forward<KeyArg>(key_arg));
	if (it == end())
	{
		return 0;
	}
	erase(it);
	return 1;
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
void openset<Key, Hash, KeyEqual, Allocator, SizeType>::erase(const iterator pos)
{
	erase(const_iterator(pos));
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
void openset<Key, Hash, KeyEqual, Allocator, SizeType>::erase(const const_iterator pos)
{
	this->hashtable_type::do_erase(pos);
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
void openset<Key, Hash, KeyEqual, Allocator, SizeType>::erase(const const_iterator first, const const_iterator last)
{
	this->hashtable_type::do_erase(first, last);
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::erase_continue(const const_iterator pos) -> iterator
{
	return this->hashtable_type::do_erase_continue(pos);
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::erase_continue(const const_iterator first, const const_iterator last) -> iterator
{
	return this->hashtable_type::do_erase_continue(first, last);
}

template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename KeyArg>
auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::count(KeyArg&& key_arg) const -> size_type
{
	return find(std::forward<KeyArg&&>(key_arg)) != end() ? 1 : 0;
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename KeyArg>
bool openset<Key, Hash, KeyEqual, Allocator, SizeType>::contains(KeyArg&& key_arg) const
{
	return find(std::forward<KeyArg&&>(key_arg)) != end();
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename KeyArg>
auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::find(KeyArg&& key_arg) -> iterator
{
	auto&& key = this->hashtable_type::do_key(std::forward<KeyArg>(key_arg));
	const hash_result hash = static_cast<const hasher&>(*this)(key);
	const find_result result = this->hashtable_type::do_find(hash, std::forward<decltype(key)>(key));
	return result.m_found
		? this->hashtable_type::index(result.m_index)
		: end();
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename KeyArg>
auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::find(KeyArg&& key_arg) const -> const_iterator
{
	auto&& key = this->hashtable_type::do_key(std::forward<KeyArg>(key_arg));
	const hash_result hash = static_cast<const hasher&>(*this)(key);
	const find_result result = this->hashtable_type::do_find(hash, std::forward<decltype(key)>(key));
	return result.m_found
		? this->hashtable_type::index(result.m_index)
		: cend();
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename KeyArg>
auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::equal_range(KeyArg&& key_arg) -> std::pair<iterator, iterator>
{
	const iterator it = find(std::forward<KeyArg>(key_arg));
	return std::make_pair(it, it);
}
template <typename Key, typename Hash, typename KeyEqual, typename Allocator, typename SizeType>
template <typename KeyArg>
auto openset<Key, Hash, KeyEqual, Allocator, SizeType>::equal_range(KeyArg&& key_arg) const -> std::pair<const_iterator, const_iterator>
{
	const const_iterator it = find(std::forward<KeyArg>(key_arg));
	return std::make_pair(it, it);
}

} // namespace sh

#endif
