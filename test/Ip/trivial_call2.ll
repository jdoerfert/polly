; RUN: opt %loadPolly -polly-interprocedural -polly-global-scops  -analyze < %s | FileCheck %s
;
;    int *A;
;    void f(int i, int k) { A[i + k] = i; }
;    void loop(int N) {
;      for (int i = 0; i < N; i++)
;        f(i, i);
;    }
;
; CHECK:         	Stmt_for_body
; CHECK-NEXT:            Domain :=
; CHECK-NEXT:                [N] -> { Stmt_for_body[i0] : 0 <= i0 < N };
; CHECK-NEXT:            Schedule :=
; CHECK-NEXT:                [N] -> { Stmt_for_body[i0] -> [1, i0, 1] };
; CHECK-NEXT:            ReadAccess :=	[Reduction Type: NONE] [Scalar: 0]
; CHECK-NEXT:                [N] -> { Stmt_for_body[i0] -> MemRef_A[0] };
; CHECK-NEXT:            MustWriteAccess :=	[Reduction Type: NONE] [Scalar: 0]
; CHECK-NEXT:                [N] -> { Stmt_for_body[i0] -> MemRef_tmp[2i0] };
;
source_filename = "trivial_call2.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

@A = common global i32* null, align 8

define void @f(i32 %i, i32 %k) {
entry:
  %tmp = load i32*, i32** @A, align 8
  %add = add nsw i32 %i, %k
  %idxprom = sext i32 %add to i64
  %arrayidx = getelementptr inbounds i32, i32* %tmp, i64 %idxprom
  store i32 %i, i32* %arrayidx, align 4
  ret void
}

define void @loop(i32 %N) {
entry:
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %i.0 = phi i32 [ 0, %entry ], [ %inc, %for.inc ]
  %cmp = icmp slt i32 %i.0, %N
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  call void @f(i32 %i.0, i32 %i.0)
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %inc = add nuw nsw i32 %i.0, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  ret void
}
