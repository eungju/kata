class Card
  KIND_RANK = [2, 3, 4, 5, 6, 7, 8, 9, 10, :J, :Q, :K, :A, 2]
  SUIT_RANK = [:CLUB, :HEART, :DIAMOND, :SPADE]
  attr_reader :suit, :kind
  def initialize(suit, kind)
    if kind
      @suit = suit
      @kind = kind
    end
  end
  def to_s
    "#{@suit}-#{kind}"
  end
  def ==(other)
    self.suit == other.suit and self.kind == other.kind
  end
  def higher?(other)
    d = KIND_RANK.index(self.kind) - KIND_RANK.index(other.kind)
    d == 0 ? SUIT_RANK.index(self.suit) > SUIT_RANK.index(other.suit) : d > 0
  end
  def same_kind?(other)
    self.kind == other.kind
  end
  def same_suit?(other)
    self.suit == other.suit
  end
  def lower_kind
    Card.new(@suit, KIND_RANK[KIND_RANK.rindex(@kind) - 1])
  end
end

def sort_cards(cards)
  cards.sort { |a,b| a.higher?(b) ? -1 : 1 }
end

def high_card(hand)
  sort_cards(hand)
end

def a_kind(hand, n)
  sorted = sort_cards(hand)
  for c in sorted
    result = sorted.select { |e| c.same_kind? e }
    if result.length >= n
      return result[0...n]
    end
  end
  []
end

def one_pair(hand)
  a_kind(hand, 2)
end

def hand_have(hand, p, result)
  if p.empty?
    result
  else
    made = p[0].call hand
    made.empty? ? [] : hand_have(hand - made, p[1..-1], result + made)
  end
end

def two_pair(hand)
  hand_have(hand, [method(:one_pair), method(:one_pair)], [])
end

def three_of_a_kind(hand)
  a_kind(hand, 3)
end

def straight(hand)
  sorted = sort_cards(hand)
  for c in sorted
    candidate = sorted.clone
    result = []
    for i in [0..4]
      x = candidate.select { |e| c.kind == e.kind }
      puts x
      if not x.empty?
        candidate = candidate - x
        result = result + x
        c = c.lower_kind
      end
    end
    if result.length >= 5
      return result
    end
  end
  []
end

def flush(hand)
  sorted = sort_cards(hand)
  for c in sorted
    result = sorted.select { |e| c.same_suit? e }
    if result.length >= 5
      return result
    end
  end
  []
end

def full_house(hand)
  hand_have(hand, [method(:three_of_a_kind), method(:one_pair)], [])
end

def four_of_a_kind(hand)
  a_kind(hand, 4)
end

def straight_flush(hand)
  flush(straight(hand))
end

class Hand
  def initialize(cards)
    @cards = cards
  end
end

require "test/unit"
require "set"
class PokerTest < Test::Unit::TestCase
  def test_kind_rank
    assert Card.new(:SPADE, :A).higher?(Card.new(:SPADE, :K))
  end
  def test_high_card
    assert_equal [Card.new(:SPADE, :A), Card.new(:DIAMOND, :A)],
                 high_card([Card.new(:DIAMOND, :A), Card.new(:SPADE, :A)])
  end
  def test_one_pair
    assert_equal [Card.new(:SPADE, :A), Card.new(:DIAMOND, :A)],
                 one_pair([Card.new(:DIAMOND, :A), Card.new(:SPADE, :A)])
  end
  def test_two_pair
    assert_equal [Card.new(:SPADE, :A), Card.new(:DIAMOND, :A), Card.new(:SPADE, :K), Card.new(:DIAMOND, :K)],
                 two_pair([Card.new(:DIAMOND, :K), Card.new(:SPADE, :K), Card.new(:DIAMOND, :A), Card.new(:SPADE, :A)])
    assert_equal [], two_pair([Card.new(:DIAMOND, :K), Card.new(:SPADE, :K)])
  end
  def test_three_of_a_kind
    assert_equal [Card.new(:SPADE, :A), Card.new(:DIAMOND, :A), Card.new(:CLUB, :A)],
                 three_of_a_kind([Card.new(:DIAMOND, :A), Card.new(:CLUB, :A), Card.new(:SPADE, :A)])
  end
  def test_straight
    assert_equal [Card.new(:CLUB, :A), Card.new(:HEART, :K), Card.new(:DIAMOND, :Q), Card.new(:SPADE, :J), Card.new(:CLUB, 10)],
                 straight([Card.new(:CLUB, :A), Card.new(:SPADE, 2), Card.new(:DIAMOND, 3), Card.new(:HEART, 4), Card.new(:CLUB, 5)])
  end
  def test_flush
    assert_equal [Card.new(:CLUB, :A), Card.new(:CLUB, :K), Card.new(:CLUB, :Q), Card.new(:CLUB, :J), Card.new(:CLUB, 10)],
                 flush([Card.new(:CLUB, 10), Card.new(:CLUB, :J), Card.new(:CLUB, :Q), Card.new(:CLUB, :K), Card.new(:CLUB, :A)])
  end
  def test_full_house
    assert_equal [Card.new(:DIAMOND, 10), Card.new(:HEART, 10), Card.new(:CLUB, 10), Card.new(:DIAMOND, 7), Card.new(:CLUB, 7)],
                 full_house([Card.new(:DIAMOND, 10), Card.new(:HEART, 10), Card.new(:CLUB, 10), Card.new(:DIAMOND, 7), Card.new(:CLUB, 7)])
    assert_equal [], full_house([Card.new(:DIAMOND, 10), Card.new(:HEART, 10), Card.new(:CLUB, 10)])
  end
  def test_four_of_a_kind
    assert_equal [Card.new(:SPADE, :A), Card.new(:DIAMOND, :A), Card.new(:HEART, :A), Card.new(:CLUB, :A)],
                 four_of_a_kind([Card.new(:CLUB, :A), Card.new(:HEART, :A), Card.new(:DIAMOND, :A), Card.new(:SPADE, :A)])
    assert_equal [], four_of_a_kind([])
  end
  def test_straight_flush
    assert_equal [Card.new(:SPADE, :A), Card.new(:SPADE, :K), Card.new(:SPADE, :Q), Card.new(:SPADE, :J), Card.new(:SPADE, 10)],
                 straight_flush([Card.new(:SPADE, 10), Card.new(:SPADE, :J), Card.new(:SPADE, :Q), Card.new(:SPADE, :K), Card.new(:SPADE, :A)])
    assert_equal [], straight_flush([])
  end
end