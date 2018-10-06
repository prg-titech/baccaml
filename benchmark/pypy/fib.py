import time

def fib(n):
    if n < 2:
        return 1
    else:
        return fib(n - 1) + fib(n - 2)


def main():
    W = 20
    N = 30

    n = 28

    wstart = time.time()
    for i in range(W):
        tstart = time.time()
        fib(n)
        print "Warmup (%d) elapsed time: %f s" % (i + 1, (time.time() - tstart))

    wend = time.time()

    print "WARMUP TIME: %f s" % ((wend - wstart) / float(W))
    start = time.time()
    for j in range(N):
        estart = time.time()
        fib(n)
        print "Execution (%d) elapsed time: %f s" % (j + 1, (time.time() - estart))

    end = time.time()
    print "TIME: %f s" % ((end - start) / float(N))


if __name__ == "__main__":
    main()
