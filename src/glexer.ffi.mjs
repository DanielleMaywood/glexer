export function slice_bytes(string, start, size) {
  return string.slice(start, start + size);
}

export function drop_byte(string) {
  return string.slice(1);
}
