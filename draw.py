import matplotlib.pyplot as plt
import matplotlib.patches as patches


# Create figure and axes
fig,ax = plt.subplots(1)

def draw_bar(minx, maxx, l, r, color):
    print("minx: %s | maxx: %s" % (minx, maxx))
    # normalize
    l = (l - minx) / float(maxx - minx)
    r = (r - minx) / float(maxx - minx)

    print("l: %s | r: %s | width: %s" % (l, r, r - l))

    Y = 0
    HEIGHT = 10
    rect = patches.Rectangle((l, 0), r - l, HEIGHT, linewidth=3, facecolor=color, alpha=0.1)
    ax.add_patch(rect)

def draw_segments(freeSegments, allbds):
    minx = 1e20
    maxx = 0
    for [l, r] in freeSegments:
        minx = min(minx, l)
        maxx = max(maxx, r)

    for [l, r, _] in allbds:
        minx = min(minx, l)
        maxx = max(maxx, r)

    for [l, r] in freeSegments:
        draw_bar(minx, maxx, l, r, 'red')


    for [l, r, live] in allbds:
        draw_bar(minx, maxx, l, r, 'green' if live else 'blue')

def show():
    plt.show()
