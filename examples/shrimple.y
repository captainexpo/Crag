fn printf(_s: *u8, ...) -> i32;
fn free(_p: *u8) -> void;
fn malloc(_size: usize) -> *u8;
fn scanf(_s: *u8, ...) -> i32;

struct Node {
  val: i32,
  next: *Node,
}

fn new_node(val: i32) -> *Node {
  let n: *Node = malloc((4 + 8) as usize) re *Node;
  n.* = Node { val: val, next: null };
  return n;
}

fn llappend(head: *Node, val: i32) -> *Node {
  let n: *Node = new_node(val); 
  if (head == null) {
    return n;
  }
  let cur: *Node = head;
  while (cur.next != null) {
    cur = cur.next;
  }
  cur.next = n;
  return head;
}

fn llprint(head: *Node) -> i32 {
  let cur: *Node = head;
  while (cur != null) {
    printf("%d ", cur.val);
    cur = cur.next;
  }
  printf("\n");
  return 0;
}

fn llfree(head: *Node) -> i32 {
  let cur: *Node = head;
  while (cur != null) {
    let next: *Node = cur.next;
    free(cur re *u8);
    cur = next;
  }
  return 0;
}

fn main(argc: i32, argv: **u8) -> i32 {
  let x: *Node = new_node(42);
  x.next = new_node(43);
  let y: *Node = x.next;
  llappend(x, 44);
  llappend(x, 45);
  llappend(x, 46);
  llprint(x);
  llfree(x);
  return 0;
}
