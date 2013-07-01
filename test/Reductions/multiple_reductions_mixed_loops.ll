; RUN: opt %loadPolly -O3 -polly -stats -polly-vectorizer=polly -polly-optimizer=isl -polly-no-tiling -polly-reduction-detection=basic -polly-code-generator=cloog -disable-output < %s 2>&1 | FileCheck %s
; ModuleID = 'multiple_reductions_mixed_loops.s'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

;  #include <stdio.h>
;
;  #define N (1024 * 8)
;
;  unsigned long A[N], B[N];
;  unsigned long sum1, sum2, mul1, mul2;
;
;  void multiple_reductions() {
;    int i, j;
;    for (i = 0; i < N; i++) {
;      for (j = 0; j < N; j++) {
;      sum1 += A[i];
;      sum2 += B[i];
;      mul1 *= A[i];
;      mul2 *= B[i];
;      }
;      sum1 += mul1;
;    }
;  }
;
;  int main() {
;    int i;
;    for (i = 0; i < N; i++) {
;      A[i] = ((i * 1337) % 1000);
;      B[i] = ((i * 42 * 77) % 500);
;      A[i] = (A[i] % 2 == 0 ? A[i] + 1 : A[i]);
;      B[i] = (B[i] % 2 == 0 ? B[i] + 1 : B[i]);
;    }
;
;    sum1 = sum2 = 0;
;    mul1 = mul2 = 1;
;
;    multiple_reductions();
;
;    printf("Sum1: %lu\n", sum1);
;    printf("Sum2: %lu\n", sum2);
;    printf("Mul1: %lu\n", mul1);
;    printf("Mul2: %lu\n", mul2);
;    return 0;
;  }

@A = common global [65536 x i64] zeroinitializer, align 16
@sum1 = common global i64 0, align 8
@B = common global [65536 x i64] zeroinitializer, align 16
@sum2 = common global i64 0, align 8
@mul1 = common global i64 0, align 8
@mul2 = common global i64 0, align 8
@.str = private unnamed_addr constant [11 x i8] c"Sum1: %lu\0A\00", align 1
@.str1 = private unnamed_addr constant [11 x i8] c"Sum2: %lu\0A\00", align 1
@.str2 = private unnamed_addr constant [11 x i8] c"Mul1: %lu\0A\00", align 1
@.str3 = private unnamed_addr constant [11 x i8] c"Mul2: %lu\0A\00", align 1

; Function Attrs: nounwind uwtable
define void @multiple_reductions() #0 {
entry:
  br label %for.cond

for.cond:                                         ; preds = %for.inc13, %entry
  %indvars.iv = phi i64 [ %indvars.iv.next, %for.inc13 ], [ 0, %entry ]
  %lftr.wideiv = trunc i64 %indvars.iv to i32
  %exitcond1 = icmp ne i32 %lftr.wideiv, 65536
  br i1 %exitcond1, label %for.body, label %for.end15

for.body:                                         ; preds = %for.cond
  br label %for.cond1

for.cond1:                                        ; preds = %for.inc, %for.body
  %j.0 = phi i32 [ 0, %for.body ], [ %inc, %for.inc ]
  %exitcond = icmp ne i32 %j.0, 65536
  br i1 %exitcond, label %for.body3, label %for.end

for.body3:                                        ; preds = %for.cond1
  %arrayidx = getelementptr inbounds [65536 x i64]* @A, i64 0, i64 %indvars.iv
  %tmp = load i64* %arrayidx, align 8
  %tmp2 = load i64* @sum1, align 8
  %add = add i64 %tmp2, %tmp
  store i64 %add, i64* @sum1, align 8
  %arrayidx5 = getelementptr inbounds [65536 x i64]* @B, i64 0, i64 %indvars.iv
  %tmp3 = load i64* %arrayidx5, align 8
  %tmp4 = load i64* @sum2, align 8
  %add6 = add i64 %tmp4, %tmp3
  store i64 %add6, i64* @sum2, align 8
  %arrayidx8 = getelementptr inbounds [65536 x i64]* @A, i64 0, i64 %indvars.iv
  %tmp5 = load i64* %arrayidx8, align 8
  %tmp6 = load i64* @mul1, align 8
  %mul = mul i64 %tmp6, %tmp5
  store i64 %mul, i64* @mul1, align 8
  %arrayidx10 = getelementptr inbounds [65536 x i64]* @B, i64 0, i64 %indvars.iv
  %tmp7 = load i64* %arrayidx10, align 8
  %tmp8 = load i64* @mul2, align 8
  %mul11 = mul i64 %tmp8, %tmp7
  store i64 %mul11, i64* @mul2, align 8
  br label %for.inc

for.inc:                                          ; preds = %for.body3
  %inc = add nsw i32 %j.0, 1
  br label %for.cond1

for.end:                                          ; preds = %for.cond1
  %tmp9 = load i64* @mul1, align 8
  %tmp10 = load i64* @sum1, align 8
  %add12 = add i64 %tmp10, %tmp9
  store i64 %add12, i64* @sum1, align 8
  br label %for.inc13

for.inc13:                                        ; preds = %for.end
  %indvars.iv.next = add i64 %indvars.iv, 1
  br label %for.cond

for.end15:                                        ; preds = %for.cond
  ret void
}

; Function Attrs: nounwind uwtable
define i32 @main() #0 {
entry:
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %indvars.iv = phi i64 [ %indvars.iv.next, %for.inc ], [ 0, %entry ]
  %lftr.wideiv = trunc i64 %indvars.iv to i32
  %exitcond = icmp ne i32 %lftr.wideiv, 65536
  br i1 %exitcond, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %tmp = mul nsw i64 %indvars.iv, 1337
  %tmp3 = trunc i64 %tmp to i32
  %rem = srem i32 %tmp3, 1000
  %conv = sext i32 %rem to i64
  %arrayidx = getelementptr inbounds [65536 x i64]* @A, i64 0, i64 %indvars.iv
  store i64 %conv, i64* %arrayidx, align 8
  %tmp4 = mul i64 %indvars.iv, 3234
  %tmp5 = trunc i64 %tmp4 to i32
  %rem3 = srem i32 %tmp5, 500
  %conv4 = sext i32 %rem3 to i64
  %arrayidx6 = getelementptr inbounds [65536 x i64]* @B, i64 0, i64 %indvars.iv
  store i64 %conv4, i64* %arrayidx6, align 8
  %arrayidx8 = getelementptr inbounds [65536 x i64]* @A, i64 0, i64 %indvars.iv
  %tmp6 = load i64* %arrayidx8, align 8
  %rem9 = and i64 %tmp6, 1
  %cmp10 = icmp eq i64 %rem9, 0
  br i1 %cmp10, label %cond.true, label %cond.false

cond.true:                                        ; preds = %for.body
  %arrayidx13 = getelementptr inbounds [65536 x i64]* @A, i64 0, i64 %indvars.iv
  %tmp7 = load i64* %arrayidx13, align 8
  %add = add i64 %tmp7, 1
  br label %cond.end

cond.false:                                       ; preds = %for.body
  %arrayidx15 = getelementptr inbounds [65536 x i64]* @A, i64 0, i64 %indvars.iv
  %tmp8 = load i64* %arrayidx15, align 8
  br label %cond.end

cond.end:                                         ; preds = %cond.false, %cond.true
  %cond = phi i64 [ %add, %cond.true ], [ %tmp8, %cond.false ]
  %arrayidx17 = getelementptr inbounds [65536 x i64]* @A, i64 0, i64 %indvars.iv
  store i64 %cond, i64* %arrayidx17, align 8
  %arrayidx19 = getelementptr inbounds [65536 x i64]* @B, i64 0, i64 %indvars.iv
  %tmp9 = load i64* %arrayidx19, align 8
  %rem20 = and i64 %tmp9, 1
  %cmp21 = icmp eq i64 %rem20, 0
  br i1 %cmp21, label %cond.true23, label %cond.false27

cond.true23:                                      ; preds = %cond.end
  %arrayidx25 = getelementptr inbounds [65536 x i64]* @B, i64 0, i64 %indvars.iv
  %tmp10 = load i64* %arrayidx25, align 8
  %add26 = add i64 %tmp10, 1
  br label %cond.end30

cond.false27:                                     ; preds = %cond.end
  %arrayidx29 = getelementptr inbounds [65536 x i64]* @B, i64 0, i64 %indvars.iv
  %tmp11 = load i64* %arrayidx29, align 8
  br label %cond.end30

cond.end30:                                       ; preds = %cond.false27, %cond.true23
  %cond31 = phi i64 [ %add26, %cond.true23 ], [ %tmp11, %cond.false27 ]
  %arrayidx33 = getelementptr inbounds [65536 x i64]* @B, i64 0, i64 %indvars.iv
  store i64 %cond31, i64* %arrayidx33, align 8
  br label %for.inc

for.inc:                                          ; preds = %cond.end30
  %indvars.iv.next = add i64 %indvars.iv, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  store i64 0, i64* @sum2, align 8
  store i64 0, i64* @sum1, align 8
  store i64 1, i64* @mul2, align 8
  store i64 1, i64* @mul1, align 8
  call void @multiple_reductions()
  %tmp12 = load i64* @sum1, align 8
  %call = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([11 x i8]* @.str, i64 0, i64 0), i64 %tmp12) #2
  %tmp13 = load i64* @sum2, align 8
  %call34 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([11 x i8]* @.str1, i64 0, i64 0), i64 %tmp13) #2
  %tmp14 = load i64* @mul1, align 8
  %call35 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([11 x i8]* @.str2, i64 0, i64 0), i64 %tmp14) #2
  %tmp15 = load i64* @mul2, align 8
  %call36 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([11 x i8]* @.str3, i64 0, i64 0), i64 %tmp15) #2
  ret i32 0
}

declare i32 @printf(i8*, ...) #1

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf"="true" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf"="true" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind }

; Check for 4 valid reductions (vectorizable) and 4 invalidated by a loop
; CHECK: 4 {{.*}} Number of loops invalidating reduction access
; CHECK: 4 {{.*}} Number of valid reduction access
; CHECK: 4 {{.*}} Number of vectorized reduction accesses
