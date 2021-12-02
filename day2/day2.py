def func(a):
    acc = [0,0,0]
    for i in a:
        if i[0] == "forward":
            acc[0] += int(i[1])
            acc[1] += acc[2]*int(i[1])
        elif i[0] == "down":
            acc[2] += int(i[1])
        else :
            acc[2] -= int(i[1])
    return acc

