; RUN: opt %loadPolly -polly-interprocedural -polly-global-scops  -analyze < %s | FileCheck %s
;
;    int *A;
;    void f(int i, int k) { A[i + k] = i; }
;    void loop(int N) {
;      for (int i = 0; i < N; i++)
;        for (int j = 0; j < N; j++)
;          f(i, j);
;    }
;
; CHECK:         	Stmt_for_body3
; CHECK-NEXT:            Domain :=
; CHECK-NEXT:                [N] -> { Stmt_for_body3[i0, i1] : 0 <= i0 < N and 0 <= i1 < N };
; CHECK-NEXT:            Schedule :=
; CHECK-NEXT:                [N] -> { Stmt_for_body3[i0, i1] -> [1, i0, 2, i1, 1] };
; CHECK-NEXT:            ReadAccess :=	[Reduction Type: NONE] [Scalar: 0]
; CHECK-NEXT:                [N] -> { Stmt_for_body3[i0, i1] -> MemRef_A[0] };
; CHECK-NEXT:            MustWriteAccess :=	[Reduction Type: NONE] [Scalar: 0]
; CHECK-NEXT:                [N] -> { Stmt_for_body3[i0, i1] -> MemRef_tmp[i0 + i1] };
;
source_filename = "trivial_call3.c"
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

for.cond:                                         ; preds = %for.inc4, %entry
  %i.0 = phi i32 [ 0, %entry ], [ %inc5, %for.inc4 ]
  %cmp = icmp slt i32 %i.0, %N
  br i1 %cmp, label %for.body, label %for.end6

for.body:                                         ; preds = %for.cond
  br label %for.cond1

for.cond1:                                        ; preds = %for.inc, %for.body
  %j.0 = phi i32 [ 0, %for.body ], [ %inc, %for.inc ]
  %exitcond = icmp ne i32 %j.0, %N
  br i1 %exitcond, label %for.body3, label %for.end

for.body3:                                        ; preds = %for.cond1
  call void @f(i32 %i.0, i32 %j.0)
  br label %for.inc

for.inc:                                          ; preds = %for.body3
  %inc = add nuw nsw i32 %j.0, 1
  br label %for.cond1

for.end:                                          ; preds = %for.cond1
  br label %for.inc4

for.inc4:                                         ; preds = %for.end
  %inc5 = add nuw nsw i32 %i.0, 1
  br label %for.cond

for.end6:                                         ; preds = %for.cond
  ret void
}
