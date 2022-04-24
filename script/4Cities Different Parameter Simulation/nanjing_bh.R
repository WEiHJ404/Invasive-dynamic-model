#Loading all necessary libraries 
pacman::p_load(
  deSolve,
  tidyverse,
  openxlsx        
)


# PARAMETERS --------------------------------------------------------------
rm(list = ls())
beta <- read.xlsx('/Users/weihj/Desktop/工作/2022/0405/南京/南京参数.xlsx', sheet = 3, cols = 1:4, colNames = T)
b <- c(beta$b)
h <- c(beta$h)
bb <- c(beta$bb)
t1 <- seq(6, 22, 2)

for (t in t1) {
  for (i in seq_along(b)) {
    
    # parameters
    b1 = 0.000000615336
    b2 = bb[i]
    h1 = 0
    h2 = h[i]
    VES1=0.5267;
    VEI1=0;
    VES2=0.7499;
    VEI2=0.07;
    x=0.45;
    k=1;
    W=1/3.66;
    W1=1/1;
    W2=1/3.66;
    p=0.31;
    y=1/4.5;
    
    # inital value
    S1= (1-x)*9310000;
    E1=0;
    Ip1=0;
    Is1=5;
    A1=0;
    Q1=0;
    R1=0;
    R2=0;
    S2=x*9310000;
    E2=0;
    Ip2=0;
    Is2=0;
    A2=0;
    Q2=0;
    R3=0;
    R4=0;
    I = 1
    
    # combining parameter and initial values
    parms <- c(b1 = b1, b2= b2, h1 = h1, h2 = h2, k = k, W = W, W1 = W1,
               W2 = W2, p = p, y = y)
    initial <- c(S1 = S1, E1 = E1, Ip1 = Ip1, Is1 = Is1, A1 = A1, R1 = R1, R2 = R2, Q1 = Q1,
                 S2 = S2, E2 = E2, Ip2 = Ip2, Is2 = Is2, A2 = A2, R3 = R3, R4 = R4, Q2 = Q2,
                 I = I)
    
    t_range <- seq(from= 0, to=1000, by=1) # vector with time steps
    
    
    # differential equations --------------------------------------------------
    
    diff_eqs <- function(times, initial, parms){
      
      with(as.list(c(initial, parms)),{
        bV <- ifelse(times <= t, b1, b2)
        hV <- ifelse(times <= t, h1, h2)
        # ifelse(22 < times < 41, h2, h3)) 
        dS1  =-bV*(1-VES1)*(1-VEI1)*S1*(Is1+k*Ip1+k*A1)-bV*(1-VES1)*(1-VEI2)*S1*(Is2+k*Ip2+k*A2)
        dE1  =bV*(1-VES1)*(1-VEI1)*S1*(Is1+k*Ip1+k*A1)+bV*(1-VES1)*(1-VEI2)*S1*(Is2+k*Ip2+k*A2)-(1-p)*W*E1-p*W2*E1
        dIp1 =(1-p)*W*E1-W1*Ip1-hV*Ip1
        dIs1 = Ip1-y*Is1-hV*Is1
        dA1  =p*W2*E1-y*A1-hV*A1
        dR1  =y*Is1+y*A1
        dR2 =y*Q1;
        dQ1 =hV*(Ip1+Is1+A1)-y*Q1;
        
        dS2  =-bV*(1-VES2)*(1-VEI2)*S2*(Is2+k*Ip2+k*A2)-bV*(1-VES2)*(1-VEI1)*S2*(Is1+k*Ip1+k*A1)
        dE2  =bV*(1-VES2)*(1-VEI2)*S2*(Is2+k*Ip2+k*A2)+bV*(1-VES2)*(1-VEI1)*S2*(Is1+k*Ip1+k*A1)-(1-p)*W*E2-p*W2*E2
        dIp2 =(1-p)*W*E2-W1*Ip2-hV*Ip2
        dIs2 =W1*Ip2-y*Is2-hV*Is2
        dA2  =p*W2*E2-y*A2-hV*A2
        dR3  =y*Is2+y*A2
        dR4 =y*Q2;
        dQ2 =hV*(Ip2+Is2+A2)-y*Q2;
        
        dI  = W1*Ip1+W1*Ip2+p*W2*E1+p*W2*E2
        
        return(list(c(dS1, dE1, dIp1, dIs1, dA1, dR1, dR2, dQ1,
                      dS2, dE2, dIp2, dIs2, dA2, dR3, dR4, dQ2,
                      dI)))
      })
    }
    
    out <- ode(initial, t_range, diff_eqs, parms, method = 'rk4')
    out <- as.data.frame(out)
    
    write.xlsx(out, paste0("/Users/weihj/Desktop/工作/2022/0413/南京/bh/",t, "/", b[i], '_', h[i], '.xlsx'),overwrite = TRUE)
    rm(out)
    # print(paste0('shanghai_output/', t, "_", b[i], '_', h[i], '.xlsx'))
  }
  
}



