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
const fadeIn = (element, step) => {
    element.hidden = false;
    return new Promise((resolve, reject) => {
        const interval = setInterval(() => {
            if (!element.style.opacity)
                element.style.opacity = "0";
            else
                element.style.opacity = (parseFloat(element.style.opacity) + step).toString();
            if (parseFloat(element.style.opacity) >= 1) {
                clearInterval(interval);
                resolve(interval);
            }
        }, 1000 / 20);
    });
};
export { fadeOut, fadeIn };
