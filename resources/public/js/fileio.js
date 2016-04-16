function myreadfile(file) {
    var reader = new FileReader();
    var text = "";
    reader.onload = function (e) {
        text = reader.result;
    }
    reader.readAsText(file);
    return reader.result;
}