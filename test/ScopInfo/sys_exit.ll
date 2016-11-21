; RUN: opt %loadPolly -analyze -polly-scops < %s | FileCheck %s
;
; Ensure "no-return" calls are supported.
;
; CHECK:        Invalid Context:
; CHECK-NEXT:   [N] -> {  : 1 = 0 }
;
;    void exit(int);
;
;    void f(int *A, int N) {
;      for (int i = 0; i < N; i++)
;        if (i == 512)
;          exit(1);
;        else
;          A[i] += 1;
;    }
;
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
  %cmp1 = icmp eq i64 %indvars.iv, 512
  br i1 %cmp1, label %if.then, label %if.else

if.then:                                          ; preds = %for.body
  call void @exit(i32 1) #0
  br label %if.end

if.else:                                          ; preds = %for.body
  %arrayidx = getelementptr inbounds i32, i32* %A, i64 %indvars.iv
  %tmp1 = load i32, i32* %arrayidx, align 4
  %add = add nsw i32 %tmp1, 1
  store i32 %add, i32* %arrayidx, align 4
  br label %if.end

if.end:                                           ; preds = %if.else
  br label %for.inc

for.inc:                                          ; preds = %if.end
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  ret void
}

declare void @exit(i32) #0

attributes #0 = { noreturn nounwind readnone }
