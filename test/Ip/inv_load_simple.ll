; RUN: opt %loadPolly -analyze < %s | FileCheck %s
;
; FIXME: Edit the run line and add checks!
;
; XFAIL: *
;
;    int Glob[7];
;
;    void system(void);
;
;    int h(void) {
;      if (Glob[0]) {
;        system();
;        return -1;
;      }
;      return Glob[1];
;    }
;
;    int g(int i) {
;      if (Glob[i])
;        system();
;      return Glob[h()];
;    }
;
;    void f(int *A, int N) {
;      for (int i = 0; i < N; i++)
;        for (int j = 0; j < i; j++)
;          A[i + j] = g(0) + h();
;    }
;
source_filename = "inv_load_simple.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

@Glob = common global [7 x i32] zeroinitializer, align 4

define i32 @h() {
entry:
  %G0inH = load i32, i32* getelementptr inbounds ([7 x i32], [7 x i32]* @Glob, i64 0, i64 0), align 4
  %tobool = icmp eq i32 %G0inH, 0
  br i1 %tobool, label %if.end, label %if.then

if.then:                                          ; preds = %entry
  call void @system() #2
  ret i32 -1

if.end:                                           ; preds = %entry, %if.then
  %G1inH = load i32, i32* getelementptr inbounds ([7 x i32], [7 x i32]* @Glob, i64 0, i64 1), align 4
  %add = add i32 %G1inH, 1
  ret i32 %add
}

declare void @system() #1

define i32 @g(i32 %i) {
entry:
  %idxprom = sext i32 %i to i64
  %arrayidxG0i = getelementptr inbounds [7 x i32], [7 x i32]* @Glob, i64 0, i64 %idxprom
  %G_i_inG = load i32, i32* %arrayidxG0i, align 4
  %tobool = icmp eq i32 %G_i_inG, 0
  br i1 %tobool, label %if.end, label %if.then

if.then:                                          ; preds = %entry
  call void @system() #2
  br label %if.end

if.end:                                           ; preds = %entry, %if.then
  %call = call i32 @h()
  %idxprom1 = sext i32 %call to i64
  %arrayidxG0h = getelementptr inbounds [7 x i32], [7 x i32]* @Glob, i64 0, i64 %idxprom1
  %G_h_inG = load i32, i32* %arrayidxG0h, align 4
  ret i32 %G_h_inG
}

define void @f(i32* %A, i32 %N) {
entry:
  %tmp = sext i32 %N to i64
  br label %for.cond

for.cond:                                         ; preds = %for.inc6, %entry
  %indvars.iv4 = phi i64 [ %indvars.iv.next5, %for.inc6 ], [ 0, %entry ]
  %cmp = icmp slt i64 %indvars.iv4, %tmp
  br i1 %cmp, label %for.body, label %for.end8

for.body:                                         ; preds = %for.cond
  br label %for.cond1

for.cond1:                                        ; preds = %for.inc, %for.body
  %indvars.iv = phi i64 [ %indvars.iv.next, %for.inc ], [ 0, %for.body ]
  %exitcond = icmp ne i64 %indvars.iv, %indvars.iv4
  br i1 %exitcond, label %for.body3, label %for.end

for.body3:                                        ; preds = %for.cond1
  %call = call i32 @g(i32 0)
  %call4 = call i32 @h()
  %add = add nsw i32 %call, %call4
  %tmp6 = add nuw nsw i64 %indvars.iv4, %indvars.iv
  %arrayidxA = getelementptr inbounds i32, i32* %A, i64 %tmp6
  store i32 %add, i32* %arrayidxA, align 4
  br label %for.inc

for.inc:                                          ; preds = %for.body3
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  br label %for.cond1

for.end:                                          ; preds = %for.cond1
  br label %for.inc6

for.inc6:                                         ; preds = %for.end
  %indvars.iv.next5 = add nuw nsw i64 %indvars.iv4, 1
  br label %for.cond

for.end8:                                         ; preds = %for.cond
  ret void
}
