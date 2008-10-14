def count(n):
    c = [1, 2, 5] + [None] * (n - 2)
    for i in xrange(3, n + 1):
        c[i] = 2 * c[i - 1] + c[i - 2] + c[i - 3]
    return c[n]

assert count(1) == 2
assert count(2) == 5
assert count(3) == 13
assert count(4) == 33
assert count(5) == 84
print 100, count(100)
print 1000, count(1000)
print 10000, count(10000)
