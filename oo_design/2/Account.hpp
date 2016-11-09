#ifndef __ACCOUNT_H_
#define __ACCOUNT_H_

class Account {
    protected:
        double balance;
    public:
        Account(double initial_deposit);
        void credit(double amount);
        bool debit(double ammount);
        double getBalance();
};

#endif

