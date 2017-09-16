import java.util.Scanner;

public class FutureInvestmentValue
{
    public static void main(String[] args)
    {

	Scanner reader = new Scanner(System.in); 
	
	System.out.println("Enter investment amount: ");
	double investmentAmount = reader.nextDouble();

	System.out.println("Enter annual interest rate:");
	double annualInterestRate = reader.nextDouble();
	double monthlyInterestRate = annualInterestRate / 12;
	
	System.out.println("Enter number of years: ");
	double numberOfYears = reader.nextDouble();

	double futureValue = investmentAmount * Math.pow(1+monthlyInterestRate, numberOfYears*12);

	System.out.print("Accumulated value is ");
	System.out.println(futureValue);

	return;
    } // main 
} // FutureInvestmentValue
