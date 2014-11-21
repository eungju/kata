ExUnit.start
defmodule MinSumTest do
  use ExUnit.Case, async: true

  def d2n(ds) do ds |> Enum.reduce(0, fn(x, acc) -> acc * 10 + x end) end
  def min_sum(ds) do
    case (ds |> Enum.sort |> Enum.partition(&(&1 != 0))) do
      {nzs, _} when length(nzs) < 2 -> -1
      {nzs, zs} ->
          ss = Enum.take(nzs, 2) ++ zs ++ Enum.drop(nzs, 2)
          as = ss |> Enum.take_every(2)
          bs = ss |> Enum.drop(1) |> Enum.take_every(2)
          d2n(as) + d2n(bs)
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
