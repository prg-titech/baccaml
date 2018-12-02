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
    for _ in range(10):
        summary(n)

    s = micros()
    for _ in range(90):
        summary(n)
    e = micros()
    return e - s


def test():
    x = 100000
    z = int(x * 0.1)
    n = 1000

    for _ in range(z):
        summary(n)

    start = micros()
    for _ in range(x):
        summary(n)
    end = micros()

    print "TIME: %f us" % ((end - start))
    return (end - start)


def bench():
    i = 100
    res = []
    for _ in range(i):
        r = test()
        res.append(r)

    print "Average: %f us, STDEV: %f" % (stc.mean(res), stc.stdev(res))


if __name__ == "__main__":
    sys.setrecursionlimit(6000)
    bench()
