import matplotlib.pyplot as plt
import matplotlib.patches as patches


# Create figure and axes
fig,ax = plt.subplots(1)

def draw_bar(minx, maxx, l, r, height, color):
    print("minx: %s | maxx: %s" % (minx, maxx))
    # normalize
    l = (l - minx) / float(maxx - minx)
    r = (r - minx) / float(maxx - minx)

    print("l: %s | r: %s | width: %s" % (l, r, r - l))

    Y = 0
    rect = patches.Rectangle((l, 0), r - l, height, linewidth=1, facecolor=color, alpha=0.4)
    ax.add_patch(rect)

def draw_tick(minx, maxx, pos, color):
    draw_bar(minx, maxx, pos, pos + 0.005 * (maxx - minx), 1.0, color)

def draw():
    minx = 1e20
    maxx = 0
    for [l, r, _] in segments:
        minx = min(minx, l)
        maxx = max(maxx, r)

    for [l, r, ty] in segments:
        if ty == "live":
            color = "blue"
            height = 0.5
        if ty == "dead":
            color = "red"
            height = 0.2
        elif ty == "heappool":
            color = "green"
            height = 0.8
        elif ty == "free":
            color = "black"
            height = 0.9

        assert color is not None
        draw_bar(minx, maxx, l, r, height, color)

    for (tickname, tickpos) in ticks:
        draw_tick(minx, maxx, tickpos, "purple")
    plt.show()
