"""
Find all the subsets from a set that are greater than or equal to some sum.
"""
def baseQ(n, b):
    """
    Convert base 10 into base b.
    """
    if n == 0:
        return [0]
    digits = []
    while n:
        digits.append(int(n % b))
        n //= b
    return digits[::-1]


def create_weights(number):
    from random import randint
    weights = []
    for i in range(number):
        weights.append(randint(1, 100 // number))
    total = sum(weights)
    leftover = 100 - total
    counter = 0
    for j in range(leftover):
        if counter % number == 0:
            counter = 0
        weights[counter] += 1
        counter += 1
    return weights



if __name__ == "__main__":
    print("Calculate subset sum")

    N = 5
    # weights = create_weights(N)
    # print(weights)
    weights = [23,17,8,34,18]
    number = len(weights)
    total = 65
    print("\nThreshold Vote",total)
    print("Maximum: {}".format(pow(2, number)))
    everything = []
    for n in range(0,pow(2, number)):
        mapping = baseQ(n, 2)
        voter = []
        while len(mapping) < number:
            mapping = [0] + mapping
        check = 0
        for index, value in enumerate(mapping):
            if value != 0:
                check += weights[index]
                voter.append(index)
            if check >= total:
                if voter not in everything:
                    print(voter)
                    everything.append(voter)
                # break
    print("\nPotential Outcomes")
    print(len(everything))
    for state in everything:
        print(state)
    
    actual = []
    while True:
        try:
            answer = int(input("\nWho Votes?\n"))
            if answer < number:
                new = []
                print("\nPotential Outcomes")
                for state in everything:
                    if answer in state:
                        if answer not in actual:
                            actual.append(answer)
                        print(state)
                        new.append(state)
                actual.sort()
                if actual in everything:
                    print("\nSOLUTION")
                    print(actual)
                    break
                everything = new
        except ValueError:
            pass
