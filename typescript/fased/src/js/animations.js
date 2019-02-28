const fadeOut = (element, step) => {
    const interval = setInterval(() => {
        if (!element.style.opacity)
            element.style.opacity = "1";
        else
            element.style.opacity = (parseFloat(element.style.opacity) - step).toString();
        if (parseFloat(element.style.opacity) <= 0) {
            clearInterval(interval);
            element.hidden = true;
        }
    }, 1000 / 20);
    return interval;
};
export { fadeOut };
