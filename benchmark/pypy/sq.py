import time
import statistics as st

def sq(n):
    m = n;
    for i in range(n):
        m += n
    return m


def bench():
    itr = 100
    i = 0
    n = 500000
    for _ in range(int(itr * 0.1)):
        sq(n)
    s = time.time()
    for _ in range(itr):
        sq(n)
    e = time.time()
    print "Time: %f" % (e - s)
    return e - s

def main():
    res = []
    for _ in range(100):
        r = bench()
        res.append(r)
    print "Average: %f, STDEV: %f" % (st.mean(res), st.stdev(res))

if __name__ == "__main__":
    main()
