
    #include <math.h>

    #define RISKFREE 0.02f
    #define VOLATILITY 0.30f

    void binomial_options(int numSteps, const float *randArray, float *output,
                          int N) {
      float puByr[N];
      float pdByr[N];
      float callA[N];
      float callB[N];

      for (int i = 0; i < N; i++) {
        float inRand = randArray[i];

        float s = (1.0f - inRand) * 5.0f + inRand * 30.f;
        float x = (1.0f - inRand) * 1.0f + inRand * 100.f;
        float optionYears = (1.0f - inRand) * 0.25f + inRand * 10.f;
        float dt = optionYears * (1.0f / (float)numSteps);
        float vsdt = VOLATILITY * sqrt(dt);
        float rdt = RISKFREE * dt;
        float r = exp(rdt);
        float rInv = 1.0f / r;
        float u = exp(vsdt);
        float d = 1.0f / u;
        float pu = (r - d) / (u - d);
        float pd = 1.0f - pu;
        puByr[i] = pu * rInv;
        pdByr[i] = pd * rInv;

        float profit = s * exp(vsdt * (2.0f * i - (float)numSteps)) - x;
        callA[i] = profit > 0 ? profit : 0.0f;
      }

      for (int i = 0; i < N; i++) {

        for (int j = numSteps; j > 0; j -= 2) {
          if (i < j) {
            callB[i] = puByr[i] * callA[i] + pdByr[i] * callA[i + 1];
          }
        }
        for (int j = numSteps; j > 0; j -= 2) {

          if (i < j - 1) {
            callA[i] = puByr[i] * callB[i] + pdByr[i] * callB[i + 1];
          }
        }
      }

      for (int i = 0; i < N; i++) {
        // write result for this block to global mem
        if (i == 0)
          output[i] = callA[0];
      }
    }

