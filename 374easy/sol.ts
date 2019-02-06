
function additivePersistence(x: number): number {
  let i = 1;
  while(true) {
    x = x.toString()
      .split('')
      .map(x => parseInt(x, 10))
      .reduce((a, acc) => a + acc, 0);
    if(x < 10) {
      break;
    }
    i++;
  }
  return i;
}

function assertEquals(x: number, y: number): void {
  if(x === y) {
  } else {
    throw new Error(`assertion failed: ${x} !== ${y}`);
  }
}

assertEquals(additivePersistence(13), 1);
assertEquals(additivePersistence(1234), 2);
assertEquals(additivePersistence(9876), 2);
assertEquals(additivePersistence(199), 3);

console.log('tests passed');
