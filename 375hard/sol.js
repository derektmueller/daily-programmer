function additivePersistence(x) {
    console.log('test');
    return 1;
}
function assert(x) {
    if (x) {
    }
    else {
        throw new Error("assertion failed");
    }
}
var a = "test";
a = null;
additivePersistence(1);
