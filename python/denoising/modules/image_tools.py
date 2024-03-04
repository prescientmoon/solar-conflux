"""Project 2 tools for generating a noisy image"""
import matplotlib.pyplot as plt
import numpy as np
from PIL import Image  # PIL is an image processing module


def create_noisy_image(path="/content/drive/My Drive/", file="sonic.jpg"):
    """Transform color image to a noisy grayscale image.
    Plot and save the transformed images.

    The file sonic.jpg must be located in your google drive and the drive must have been
    "mounted" with:

        from google.colab import drive
        drive.mount("/content/drive")
    """
    image = Image.open(path + file)
    # Transform RGB image to grayscale
    image = image.convert("L")

    # get the filename without the extension
    # (note: the pathlib module offers better ways to do this!)
    name = file.removesuffix(".jpg")

    image.save(path + name + "_gray.jpg")

    # set images to be plotted in gray scale
    plt.gray()
    plt.imshow(image)
    plt.title("Original image")
    plt.show()

    # add noise to grayscale image
    sigma = 2.0
    img_arr = np.array(image)
    img_arr_noisy = np.clip(
        img_arr + sigma * np.random.normal(0, 100, img_arr.shape), 0, 255
    ).astype(np.uint8)
    plt.title("Noisy image")
    plt.imshow(img_arr_noisy)

    # create Image object from numpy array and save to disk
    image = Image.fromarray(img_arr_noisy, mode="L")
    image.save(path + name + "_gray_noisy.jpg")

    plt.show()
