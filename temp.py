
def func1(n5):
    if n5 > 0:
        return min(n5*250, 5)
    else:
        return max(n5*250, -5)

def goal_calc(n5, l5, h5):

    if not n5:
        return ""
    elif not l5:
        return h5*func1(n5)
    elif bool(l5):
        if l5 > 0.6:
            return min((7.1429*l5) - 4.2857, 5)
        elif l5 <= 0.6:
            return max((4.1667*l5)-2.5, -5)

# if ratio doesn't exist then observed growth is used (way to distinguish from target/no target)
# so if cagr_o = "" then there is no  goal level calculation
n5 = 0.002496266 # value of cagr calculation with no quantitative target
l5 = None # value of cagr calculation with quantitative target
h5 = 1 # desired direction of progress

print(goal_calc(n5, l5, h5))