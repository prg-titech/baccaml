import sys
import time

def summary(n):
    if n == 0: return 0
    else: return n + summary(n -1)


def main():
    W = 10
    N = 100

    n = 5000

    res = 0

    wstart = time.time()
    for i in range(W):
        tstart = time.time()
        res = summary(n)
        print "%f" % ((time.time() - tstart) * 1000000.0)

    wend = time.time()

    print "WARMUP TIME: %f s" % ((wend - wstart) / float(W))
    start = time.time()
    for j in range(N):
        estart = time.time()
        res = summary(n)
        print "%f" % ((time.time() - estart) * 100000.0)

    end = time.time()
    print "TIME: %f s" % ((end - start) / float(N))


if __name__ == "__main__":
    sys.setrecursionlimit(10000)
    main()
