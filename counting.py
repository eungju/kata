def count(N):
    C = [1, 2, 5] + [None] * (N - 2)
    for i in xrange(3, N + 1):
        C[i] = 2 * C[i - 1] + C[i - 2] + C[i - 3]
    return C[N]

assert count(2) == 5
assert count(3) == 13
assert count(4) == 33
assert count(5) == 84
print 100, count(100)
print 1000, count(1000)
print 10000, count(10000)
