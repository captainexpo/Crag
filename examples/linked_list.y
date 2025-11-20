extern fn printf(_s: *u8, ...) -> i32;
extern fn free(_p: *u8) -> void;
extern fn malloc(_size: usize) -> *u8;
extern fn scanf(_s: *u8, ...) -> i32;

struct Node {
  val: i32,
  next: *Node,

  fn print(self: *Node) -> void {
    if (self.next == null) {
      printf("%d\n", self.val);
      return;
    }
    printf("%d -> ", self.val);
    self.next.print();
  }

  fn append(self: *Node, val: i32) -> void {
    let n = new_node(val);
  }
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
  x.append(44);
  x.append(45);
  x.append(46);
  x.print();
  llfree(x);
  return 0;
}
