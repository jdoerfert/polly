; RUN: opt %loadPolly -polly-interprocedural -polly-global-scops -analyze < %s | FileCheck %s
;
;    void f(int *A, int N) {
;      for (int i = 0; i < N; i++) {
;        A[i] += i;
;      }
;      int mid = N / 2;
;      if (N > 2)
;        f(A, mid);
;      else
;        f(A + mid, mid);
;    }
;
; CHECK:         	Stmt_for_body
; CHECK-NEXT:            Domain :=
; CHECK-NEXT:                [N] -> { Stmt_for_body[i0] : 0 <= i0 < N };
; CHECK-NEXT:            Schedule :=
; CHECK-NEXT:                [N] -> { Stmt_for_body[i0] -> [1, i0, 1] };
; CHECK-NEXT:            ReadAccess :=	[Reduction Type: +] [Scalar: 0]
; CHECK-NEXT:                [N] -> { Stmt_for_body[i0] -> MemRef_A[i0] };
; CHECK-NEXT:            MustWriteAccess :=	[Reduction Type: +] [Scalar: 0]
; CHECK-NEXT:                [N] -> { Stmt_for_body[i0] -> MemRef_A[i0] };
; CHECK-NEXT:    	Stmt_if_then
; CHECK-NEXT:            Domain :=
; CHECK-NEXT:                [N] -> { Stmt_if_then[] : N >= 3 };
; CHECK-NEXT:            Schedule :=
; CHECK-NEXT:                [N] -> { Stmt_if_then[] -> [4, 0, 0] };
; CHECK-NEXT:            ReadAccess :=	[Reduction Type: NONE] [Scalar: 0]
; CHECK-NEXT:                [N] -> { Stmt_if_then[] -> MemRef_A[o0] : o0 >= 0 and 2o0 <= -2 + N };
; CHECK-NEXT:            MustWriteAccess :=	[Reduction Type: NONE] [Scalar: 0]
; CHECK-NEXT:                [N] -> { Stmt_if_then[] -> MemRef_A[o0] : o0 >= 0 and 2o0 <= -2 + N };
; CHECK-NEXT:    	Stmt_if_else
; CHECK-NEXT:            Domain :=
; CHECK-NEXT:                [N] -> { Stmt_if_else[] : N <= 2 };
; CHECK-NEXT:            Schedule :=
; CHECK-NEXT:                [N] -> { Stmt_if_else[] -> [3, 0, 0] };
; CHECK-NEXT:            ReadAccess :=	[Reduction Type: NONE] [Scalar: 0]
; CHECK-NEXT:                [N] -> { Stmt_if_else[] -> MemRef_A[o0] : 2o0 >= -1 + N and 2*floor((1 - N)/2) <= -2 + N - 2o0 };
; CHECK-NEXT:            MustWriteAccess :=	[Reduction Type: NONE] [Scalar: 0]
; CHECK-NEXT:                [N] -> { Stmt_if_else[] -> MemRef_A[o0] : 2o0 >= -1 + N and 2*floor((1 - N)/2) <= -2 + N - 2o0 };
; CHECK-NEXT:    	Stmt_if_end [ReturnStmt]
; CHECK-NEXT:            Domain :=
; CHECK-NEXT:                [N] -> { Stmt_if_end[] };
; CHECK-NEXT:            Schedule :=
; CHECK-NEXT:                [N] -> { Stmt_if_end[] -> [5, 0, 0] };
; CHECK-NEXT:            ReadAccess :=	[Reduction Type: NONE] [Scalar: 0]
; CHECK-NEXT:                [N] -> { Stmt_if_end[] -> MemRef_A[o0] };
;
source_filename = "recursive_1.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

define void @f(i32* %A, i32 %N) {
entry:
  %tmp = sext i32 %N to i64
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %indvars.iv = phi i64 [ %indvars.iv.next, %for.inc ], [ 0, %entry ]
  %cmp = icmp slt i64 %indvars.iv, %tmp
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %arrayidx = getelementptr inbounds i32, i32* %A, i64 %indvars.iv
  %tmp1 = load i32, i32* %arrayidx, align 4
  %tmp2 = trunc i64 %indvars.iv to i32
  %add = add nsw i32 %tmp1, %tmp2
  store i32 %add, i32* %arrayidx, align 4
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  %div = sdiv i32 %N, 2
  %cmp1 = icmp sgt i32 %N, 2
  br i1 %cmp1, label %if.then, label %if.else

if.then:                                          ; preds = %for.end
  call void @f(i32* %A, i32 %div)
  br label %if.end

if.else:                                          ; preds = %for.end
  %idx.ext = sext i32 %div to i64
  %add.ptr = getelementptr inbounds i32, i32* %A, i64 %idx.ext
  call void @f(i32* %add.ptr, i32 %div)
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  ret void
}
