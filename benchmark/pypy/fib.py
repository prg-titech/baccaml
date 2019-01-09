import time
import statistics as st

def fib(n):
    if n < 2:
        return 1
    else:
        return fib(n - 1) + fib(n - 2)


def main():
    W = 10
    N = 90

    n = 40

    res = 0

    wstart = time.time()
    for i in range(W):
        tstart = time.clock()
        res = fib(n)
        tend = time.clock()
        print (tend - tstart)

    wend = time.time()

    print "WARMUP TIME: %f s" % ((wend - wstart) / float(W))
    start = time.clock()
    for j in range(N):
        tstart = time.clock()
        res = fib(n)
        tend = time.clock()
        print (tend - tstart)

    end = time.clock()
    print "TIME: %f s" % ((end - start) / float(N))


if __name__ == "__main__":
    main()
