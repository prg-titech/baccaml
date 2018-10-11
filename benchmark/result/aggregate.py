import statistics as st
import test_fib as tf
import test_fib28 as tf28

def average(lst):
    return sum(lst) / float(len(lst))


def variance(lst):
    return st.variance(lst)


def relative(lhs, rhs):
    return map(lambda (x, y): x / float(y), zip(lhs, rhs))


mj_fib40_mc = relative(tf.res_fib_mj, tf.res_fib_mincaml)
tj_fib40_mc = relative(tf.res_fib_tj, tf.res_fib_mincaml)
ip_fib40_mc = relative(tf.res_fib_interp, tf.res_fib_mincaml)
c_fib40_mc = relative(tf.res_fib_c, tf.res_fib_mincaml)
pypy_fib40_mc = relative(tf.res_fib_pypy, tf.res_fib_mincaml)


mj_fib28_mc = relative(tf28.res_fib28_mj, tf28.res_fib28_mincaml)
mj_fib28_mc = relative(tf28.res_fib28_tj, tf28.res_fib28_mincaml)
mj_fib28_mc = relative(tf28.res_fib28_interp, tf28.res_fib28_mincaml)
mj_fib28_mc = relative(tf28.res_fib28_c, tf28.res_fib28_mincaml)
mj_fib28_mc = relative(tf28.res_fib28_pypy, tf28.res_fib28_mincaml)


def print_result(name, lst, unit='s'):
    if unit is 's':
        print "%s: \tAverage %f s \tVariance %f \t STDEV %f" % (name, average(lst), variance(lst), st.stdev(lst))
    elif unit is 'us':
        print "%s: \tAverage %f us \tVariance %f \t STDEV %f" % (name, average(lst), variance(lst), st.stdev(lst))


def calc_fib40():
    from test_fib import *
    print "fib(40)"
    print_result("Method JIT", res_fib_mj)
    print_result("Interpreter", res_fib_interp)
    print_result("MinCaml", res_fib_mincaml)
    print_result("C (clang)", res_fib_c)
    print_result("PyPy (6.0)", res_fib_pypy)


def calc_fib28():
    from test_fib28 import *
    print "fib(28)"
    print_result("Method JIT", res_fib28_mj)
    print_result("Tracing JIT", res_fib28_tj)
    print_result("Interpreter", res_fib28_interp)
    print_result("MinCaml", res_fib28_mincaml)
    print_result("C (clang)", res_fib28_c)
    print_result("PyPy (6.0)", res_fib28_pypy)


def calc_sum():
    import test_sum as ts
    print "sum(5000)"
    print_result("Method JIT", ts.res_sum_mj, unit='us')
    print_result("Tracing JIT", ts.res_sum_tj, unit='us')
    print_result("Interpreter", ts.res_sum_interp, unit='us')
    print_result("MinCaml", ts.res_sum_mincaml, unit='us')
    print_result("C (clang)", ts.res_sum_c, unit='us')
    print_result("PyPy (6.0)", ts.res_sum_pypy, unit='us')


def calc():
    calc_fib40()
    calc_fib28()
    calc_sum()


def main():
    calc()


main()
