; RUN: opt %loadPolly -polly-interprocedural   -polly-global-scops -analyze < %s | FileCheck %s
;
;    void f(int *A, int i, int k) { A[i + k] = i; }
;
;    int *G;
;    void loop(int N) {
;      for (int i = 0; i < N; i++)
;        f(G, i, i);
;    }
;
; CHECK:      Alias Groups (1):
; CHECK-NEXT: <[N] -> { MemRef_G[(0)] : N > 0 }, [N] -> { MemRef_G[(1)] : N > 0 }> <[N] -> { MemRef_tmp[(0)] : N > 0 }, [N] -> { MemRef_tmp[(-1 + 2N)] : N > 0 }>
;
; CHECK:      Stmt_for_body
; CHECK-NEXT: Domain :=
; CHECK-NEXT:   [N] -> { Stmt_for_body[i0] : 0 <= i0 < N };
; CHECK-NEXT: Schedule :=
; CHECK-NEXT:   [N] -> { Stmt_for_body[i0] -> [1, i0, 1] };
; CHECK-NEXT: MustWriteAccess := [Reduction Type: NONE] [Scalar: 0]
; CHECK-NEXT:   [N] -> { Stmt_for_body[i0] -> MemRef_tmp[2i0] };
;
source_filename = "call4.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

@G = common global i32* null, align 8

define void @f(i32* %A, i32 %i, i32 %k) {
entry:
  %add = add nsw i32 %i, %k
  %idxprom = sext i32 %add to i64
  %arrayidx = getelementptr inbounds i32, i32* %A, i64 %idxprom
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
  %tmp = load i32*, i32** @G, align 8
  call void @f(i32* %tmp, i32 %i.0, i32 %i.0)
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %inc = add nuw nsw i32 %i.0, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  ret void
}
