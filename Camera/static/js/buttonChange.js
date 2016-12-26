function buttonChange (obj) {
    if (obj.style.background) {
	obj.style.background = null;
    } else {
	obj.style.background = 'pink';
    }
}

function curWrite() {
    var table = document.getElementById("kumiai");
    var rowMax = table.rows.length;

    for (var i = 0; i < rowMax; i++) {
	var line = table.getElementsByClassName("linear" + String(i));
	var han = line.item(1).innerText;
	var name = line.item(2).innerText;
	var multiple = line.item(3).firstChild.value;
	var tel = "";
	for (var col = 4; col < 9; col++) {
	    var column = line.item(col);
	    if (column) {
		var but = column.firstChild;
		if (but.style.background) {
		    tel = tel + column.firstChild.value + "・";
		}
	    }
	}
	alert(han + name + multiple + tel);
    }
}

function change(current) {
    var inputs = document.getElementsByClassName("numbers");
    var sum = 0;
    for (var i = 0; i < inputs.length; i++) {
	sum = sum + Number(inputs.item(i).value);
    }
    ratio = Math.round((current - sum) * 1000 / current) / 10.0
    note = document.getElementsByClassName("active").item(0)
    note.innerText = "滞納者" + String(sum) + "人 現勢" + String(current) + "人 推定納入率" + String(ratio) + "%";
}
