const terminal = document.getElementById("terminal");
function print(mes, eol = true) {
    console.log(mes);
    let finalMes;
    if (mes)
        finalMes = mes.toString();
    else
        finalMes = "null";
    if (finalMes == "[object Object]")
        finalMes = JSON.stringify(mes);
    terminal.innerHTML += `${eol ? "<br/>" : ""}${finalMes}`;
}
export { print };
