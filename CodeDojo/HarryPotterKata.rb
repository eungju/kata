require 'test/unit'

DISCOUNT_RATES = [1, 1, 0.95, 0.9, 0.8, 0.75]

def totalPrice(books)
  c = combination(books, [])
  return c.collect {|packs|
    price = 0
    packs.each {|pack| price += 8 * pack.length * DISCOUNT_RATES[pack.length]}
    price
  }.min
end

def combination(books, packs)
  if books.empty?
    return [packs]
  end
  xxx = []
  if books.length > 8
    nextPack = books.uniq
    p = packs.clone
    b = books.clone
    p.push(nextPack)
    nextPack.each {|item| b.delete_at(b.index(item))}
    xxx.concat(combination(b, p))
  else
    generate(books.uniq).delete_if {|nextPack|nextPack.length * 2 < books.uniq.length}.each {|nextPack|
      p = packs.clone
      b = books.clone
      p.push(nextPack)
      nextPack.each {|item| b.delete_at(b.index(item))}
      xxx.concat(combination(b, p))
    }
   end
  return xxx
end

def generate(set)
  subsets = []
  (2 ** set.length - 1).downto(1) {|combination|
    subset = []
    set.length.times {|index|
      if not (combination & (1 << index)).zero?
        subset.push(set[index])
      end
    }
    subsets.push(subset)
  }
  return subsets.reverse
end

class HarryPotterKataTest < Test::Unit::TestCase
  def testGenerate
    assert_equal([], generate([]))
    assert_equal([[1]], generate([1]))
    assert_equal([[1], [2], [1, 2]], generate([1, 2]))
    assert_equal([[1], [2], [1, 2], [3], [1, 3], [2, 3], [1, 2, 3]], generate([1, 2, 3]))
  end
  def testBasicCases
    assert_equal(0, totalPrice([]))
    assert_equal(8, totalPrice([0]))
    assert_equal(8, totalPrice([1]))
    assert_equal(8, totalPrice([2]))
    assert_equal(8, totalPrice([3]))
    assert_equal(8, totalPrice([4]))
    assert_equal(8 * 2, totalPrice([0, 0]))
    assert_equal(8 * 3, totalPrice([1, 1, 1]))
  end
  def testSimpleDiscounts
    assert_equal(8 * 2 * 0.95, totalPrice([0, 1]))
    assert_equal(8 * 3 * 0.9, totalPrice([0, 2, 4]))
    assert_equal(8 * 4 * 0.8, totalPrice([0, 1, 2, 4]))
    assert_equal(8 * 5 * 0.75, totalPrice([0, 1, 2, 3, 4])) 
  end
  def testSeveralDiscountsCombined
    assert_equal(8 + (8 * 2 * 0.95), totalPrice([0, 0, 1])) 
    assert_equal(2 * (8 * 2 * 0.95), totalPrice([0, 0, 1, 1])) 
    assert_equal((8 * 4 * 0.8) + (8 * 2 * 0.95), totalPrice([0, 0, 1, 2, 2, 3])) 
    assert_equal(8 + (8 * 5 * 0.75), totalPrice([0, 1, 1, 2, 3, 4])) 
  end
  def testViciousCases
    assert_equal(2 * (8 * 4 * 0.8), totalPrice([0, 0, 1, 1, 2, 2, 3, 4]))
    assert_equal(3 * (8 * 5 * 0.75) + 2 * (8 * 4 * 0.8), 
      totalPrice([0, 0, 0, 0, 0, 
                  1, 1, 1, 1, 1, 
                  2, 2, 2, 2, 
                  3, 3, 3, 3, 3, 
                  4, 4, 4, 4]))
  end
end