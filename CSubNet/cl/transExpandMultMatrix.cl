__kernel void transExpandMatrixMult(__global const float *left, __global const float *right, __global float *target,
  const int leftWidth, const int rightHeight) { 
  int leftRow = get_global_id(0);
  
  int rightRow;
  for (rightRow = 0; rightRow < rightHeight; rightRow++) {
    float sum = 0;
    int n;
    for(n = 0; n < leftWidth; n++) {
      sum += left[leftRow * leftWidth + n] * right[rightRow * leftWidth + n];
    }
    target[leftRow * rightHeight + rightRow] = sum;
  }
}