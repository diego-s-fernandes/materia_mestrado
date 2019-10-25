
p <- 0.95;
q1 <- -qnorm((1-p)/2);              #---- normal 
q2 <- -qt((1-p)/2, df=19);          #---- n = 20 
q3 <- -qt((1-p)/2, df=29);          #---- n = 29 
q4 <- -qt((1-p)/2, df=10000);    # converge para normal

q1; q2; q3; q4

q1 <- -qt((1-0.95)/2, df=14)
q2 <- -qt((1-0.99)/2, df=14)
q3 <- -qt((1-0.90)/2, df=14)

q1; q2; q3;

amostra <- c(2.3, 1.5, 2.1, 6.3, 2.2, 6.1, 5.3, 0.8, 8.1, 4.12,
             0.2, 3.12, 7.42, 8.3, 15.6, 9.6, 2.3, 21.3, 4.3, 2.8);

t.test(amostra); #---- int. confianca com 95% de prob. de cobertura
t.test(amostra, conf.level = 0.90);
t.test(amostra, conf.level = 0.99);






b