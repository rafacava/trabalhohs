window.onload = header;
function header(){
    var h1 = document.createElement("h1");
    var h1txt = "testando 1 2 3!";
    var diva = document.createElement("div");
    diva.setAttribute("class", "container");;
    var formula = document.getElementById("formula");
    h1.appendChild(h1txt);
    document.body.appendChild(h1);
    document.body.appendChild(diva);
    diva.appendChild(formula);
}