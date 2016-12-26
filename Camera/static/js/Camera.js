function changeColorOver(obj, color) {
    obj.style.backgroundColor = color;
}

function mainTable() {
    return document.getElementById("MainTable");
}

function getCheckbox(bunkai) {
    var ret = bunkai + "&";
    var table = mainTable();
    for (var i = 0; i < table.rows.length; i++) {
	if (document.forms.CheckBox[i].checked == true) {
	    ret = ret + String(i) + "&";
	}
    }
    window.open("/csv/" + ret, '_blank')
}

function checkFunc (obj) {
    var table = mainTable();
    for (var i = 0; i < table.rows.length + 2; i++) {
        var row = table.rows[i];
	var j   = i + 2;
	if (document.forms.CheckBox[j].checked == true) {
	    row.style.backgroundColor = "pink";
	} else {
	    row.style.backgroundColor = "white";
	}
    }
}

function release () {
    var table = mainTable();
    for (var i = 0; i < table.rows.length; i++) {
        document.forms.CheckBox[i].checked = false;
        table.rows[i].style.backgroundColor = "white";
    }
}
