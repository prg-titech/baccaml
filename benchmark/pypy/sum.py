import sys
import time
import statistics as stc


def micros():
    return int(time.time() * 100000.0)


def summary(n):
    if n == 0:
        return 0
    else:
        return n + summary(n - 1)


def bench_sum(n):
    for _ in range(100):
        summary(n)


def test():
    N = 100000
    n = 1000

    start = micros()
    for j in range(N):
        summary(n)
    end = micros()

    print "TIME: %f us" % ((end - start))


def bench():
    i = 100
    res = []
    for _ in range(i):
        s = micros()
        test()
        e = micros()
        res.append(e - s)

    print "Average: %f us, STDEV: %f" % (stc.mean(res), stc.stdev(res))


if __name__ == "__main__":
    sys.setrecursionlimit(6000)
    bench()
