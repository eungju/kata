ExUnit.start
defmodule MinSumTest do
  use ExUnit.Case, async: true

  def min_num([], acc, h, o) do div(h * o + acc, 10) end
  def min_num([0|dt], acc, h, o) do min_num(dt, acc, h, o * 10) end
  def min_num([dh|dt], acc, h, o) do min_num(dt, h * o + acc, dh, o * 10) end
  def min_num(ds) do min_num(ds, 0, 0, 1) end

  def split([], as, bs) do {as, bs} end
  def split([a], as, bs) do split([], [a|as], bs) end
  def split([0,b,c|t], as, bs) when b != 0 do split(t, [b,0|as], [c|bs]) end
  def split([a,b|t], as, bs) do split(t, [a|as], [b|bs]) end
  def split(is) do split(is |> Enum.sort, [], []) end

  def min_sum(is) do
    {as, bs} = split(is)
    case {min_num(as),  min_num(bs)} do
      {_, 0} -> -1
      {m, n} -> m + n
    end
  end

  test "simple cases" do
    assert 176 = min_sum([1, 2, 4, 7, 9])
    assert 246 = min_sum([1, 2, 3, 1, 2, 3])
    assert 1603 = min_sum([1, 2, 3, 4, 5, 6, 7])
    assert 11257 = min_sum([0, 1, 2, 3, 0, 1, 2, 3, 4])
  end

  test "invalid inputs" do
    assert -1 = min_sum([0, 0, 1])
    assert -1 = min_sum([0, 0])
  end

  test "short inputs" do
    assert 2 = min_sum([1, 1])
    assert 12 = min_sum([1, 1, 1])
    assert 13 = min_sum([1, 1, 2])
  end

  test "handle 0s properly" do
    assert 11 = min_sum([0, 1, 1])
    assert 12 = min_sum([0, 1, 2])
    assert 20 = min_sum([0, 0, 1, 1])
    assert 30 = min_sum([0, 0, 1, 2])
    assert 110 = min_sum([0, 0, 0, 1, 1])
    assert 120 = min_sum([0, 0, 0, 1, 2])
  end
end
