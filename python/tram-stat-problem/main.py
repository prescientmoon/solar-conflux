import numpy as np
import matplotlib.pyplot as plt

size = 100000

fig, axes = plt.subplots(3, 3, figsize=(15, 15))
fig.subplots_adjust(hspace=0.5)


def check(label, tram, ax):
    bugun = np.random.uniform(0, 100, size=size)
    tram = np.clip(tram, 0, 100)

    result = (tram - 10 <= bugun) & (bugun < tram)
    count = np.sum(result)

    print(f"{label}: {count}")

    # ax.hist(bugun, alpha=0.5, label="Bugun", histtype="step", bins="auto")
    # ax.hist(tram, alpha=0.5, label="Tram", histtype="step", bins="auto")
    ax.hist(tram - bugun, label="Difference", bins="auto")
    ax.set_title(label)
    ax.legend()


distributions = [
    ("uniform", np.random.uniform(0, 100, size=size)),
    ("normal(1)", np.random.normal(50, 1, size=size)),
    ("normal(10)", np.random.normal(50, 10, size=size)),
    ("normal(30)", np.random.normal(50, 30, size=size)),
    ("normal(50)", np.random.normal(50, 50, size=size)),
    ("normal(70)", np.random.normal(50, 70, size=size)),
    ("normal(100)", np.random.normal(50, 100, size=size)),
    ("normal(10000)", np.random.normal(50, 10000, size=size)),
]

for i in range(len(distributions)):
    check(*distributions[i], axes.flat[i])

plt.show()
