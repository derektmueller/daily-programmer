
function balanced(s: string): boolean {
  const {x, y} = Array.from(s).reduce((acc, c) => {
    if(c === 'x') {
      return {...acc, x: acc.x + 1};
    } else {
      return {...acc, y: acc.y + 1};
    }
  }, {x: 0, y: 0});
  return x === y;
}

function assertTrue(val: boolean): void {
  if(val) {
  } else {
    throw new Error(`assertion ${val} !== true`);
  }
}

function assertFalse(val: boolean): void {
  if(!val) {
  } else {
    throw new Error(`assertion ${val} !== false`);
  }
}

assertTrue(balanced("xxxyyy"));
assertTrue(balanced("yyyxxx"));
assertFalse(balanced("xxxyyyy"));
assertTrue(balanced("yyxyxxyxxyyyyxxxyxyx"));
assertTrue(balanced(""));
assertFalse(balanced("x"));

console.log('tests passed');
