from tkinter import *

def Try():  ##Function for Truth Table 
    master.destroy()   ##Destroys master window after try function has been called
    global root,exp1, entry1     
    root=Tk()   ##Creating Window
    root.wm_title("TRUTH TABLES")   ##CHANGING THE tiltle caption
    exp1 = StringVar()
    entry1 = Entry(root,textvariable = exp1)
    Instructions_Label = Label(root, text = "INSTRUCTIONS",bg="tan")   ##label
    Instructions_Label.pack()    ##Making label visible
    Instructions = Label(root,fg = 'DeepPink4', text = "You can only use p q and r operators\nFor Operations Type: 'or' 'and' 'not'\nfor xor: Use ^\nFor p ↔ q:(p → q) and (q → p) \nFor p → q: Use (not(p) or q)\n ")   ##label
    Instructions.pack()    ##Making label visible
    Input = Label(root, text = 'Your Input Comes Here')     ##label for Conclusion
    Input.pack()    ##Making label visible
    entry1.pack()

    def MainMenu():     ##Main Menu Funtion        root.destroy()  ##Destroying the root
        main()          ##Calling main Funtion

    def TruthThree():   ##Truth table for 3 inputs
        global exp1, entry1
        data = exp1.get()   ##Taking data to evaluate

        ##Inputs Defined
        P = [0,0,0,0,1,1,1,1]
        Q = [0,0,1,1,0,0,1,1]
        R = [0,1,0,1,0,1,0,1]
        
        if not exp1.get():
            res = Label(root, text = "Please Insert the data first")   ##Creating the label
            res.pack()  ##Making label visible
        else:
            lab4 = Label(root, text = 'P   Q   R   Output')     ##label
            lab4.pack()    ##Making label visible
        print(data)
        print('P   Q   R   Output')
        for i in range (8):
             
            p = P[i]    ##inputs converted into smaller letters
            q = Q[i]
            r = R[i]

            a=str(p)    ##Inputs converted into Strings
            b=str(q)
            c=str(r)
            k =  eval(data)
            if k == False:   ##Changing the True or False into integers
                k = int(False)
            else:
                k = int (True)
            ans1=str(k)   ##Evaliuating the data
            
            res = Label(root, text = a+"    "+b+"     "+c+"       "+ans1)   ##Creating the label
            res.pack()  ##Making label visible
            print(P[i],' ',Q[i],' ',R[i],'  ', ans1) ##Prinitng on console
         

    def TruthTwo():        ##Truth table for 2 inputs
         global exp1, entry1
         P = [0,0,1,1]  ##Inputs Defined
         Q = [0,1,0,1]
         
         data = exp1.get()
         if not exp1.get():
            res = Label(root, text = "Please Insert the data first")   ##Creating the label
            res.pack()  ##Making label visible
         else:
             lab4 = Label(root, text = 'P   Q   Output')     ##label
             lab4.pack()    ##Making label visible
         print(data)
         print('P   Q   R   Output')
         for i in range (4):
             
             p = P[i]   ##inputs converted into smaller letters
             q = Q[i]

             a=str(p)    ##Inputs conveted into Strings
             b=str(q)
             k =  eval(data)
             if k == False:   ##Changing the True or False into integers
                 k = int(False)
             else:
                 k = int (True)
             ans1=str(k)   ##Evaluating the data
             res = Label(root, text = a+"    "+b+"       "+ans1)     ##Creating the label
             res.pack()  ##Making label visible

             print(P[i],' ',Q[i],' ',ans1)  ##Prinitng on console

    Free_label = Label(root, text = "")   ##label
    Free_label.pack()    ##Making label visible
    button_Truth_two = Button(root,padx=43, pady=10,bg="powder blue", text = "Two Operator's Result", command=TruthTwo) ##Button to get result of 2 inputs truth table
    button_Truth_two.pack()##Generating Button

    Free_label = Label(root, text = "")   ##label
    Free_label.pack()    ##Making label visible
    button_Truth_three = Button(root,padx=40, pady=10,bg="powder blue", text = "Three Operator's Result", command=TruthThree) ## Button to get 3 input truth table result
    button_Truth_three.pack()##Generating Button

    Free_label = Label(root, text = "")   ##label
    Free_label.pack()    ##Making label visible
    button_Truth_two = Button(root,padx=70, pady=15,bg="powder blue", text = "Main Menu", command=MainMenu) ##Button that leads to main menu
    button_Truth_two.pack()##Generating Button
    root.mainloop()

#Three Operator function starts
def ThreeFunctions():
    master.destroy()    ##Deallocating the main menu
    root=Tk()    ##Creation of new Window
    root.geometry("280x500")    ##Setting window size
    root.wm_title("CALCULATOR FOR 3 OPERATORS") ##cHANGING THE tiltle caption

    ##Labels
    lab_valid = Label(root, text = "VALID")
    lab_invalid = Label(root, text = "INVALID")
    lab1 = Label(root, text = "Expression 1")   ##label for expression 1
    lab2 = Label(root, text = "Expression 2")   ##label for expression 2
    lab3 = Label(root, text = "Expression 3")   ##label for expression 3
    lab4 = Label(root, text = "Conclusion")     ##label for Conclusion

    def MainMenu():     ##Main Menu Funtion
        root.destroy()  ##Destroying the root
        main()          ##Calling main Funtion

    ##Lists to show possible combination of 3-input Truth tables
    P = [0,0,0,0,1,1,1,1]
    Q = [0,0,1,1,0,0,1,1]
    R = [0,1,0,1,0,1,0,1]
    ##Empty list to append the results in order to check validity 
    lst1=[]
    lst2=[]
    lst3=[]
    conclude=[]
    #expression calculation
    def expression1():
         del lst1[:]    ##Deleting the list
         data = exp1.get()  ##Taking data to evaluate
         print(data)
         print('P',' ','Q',' ','R','  ','Output' )  ##labels
         for i in range (8):
             p = P[i]   ##input operators decleared
             q = Q[i]
             r = R[i]
             
             ans = eval(data)    ##Evalutes whatever expressions that has been given in data
                
             if ans == False:   ##Changing the True or False into integers
                 ans = int(False)
             else:
                 ans = int (True)
             lst1.append(ans) 
             print(P[i],' ',Q[i],' ',R[i],'  ', ans)
         print("List 1: ",lst1)

    #expression calculation
    def expression2(): 
         del lst2[:]     ##Deleting the list
         data = exp2.get()   ##Taking data to evaluate
         print(data)
         print('P',' ','Q',' ','R','  ','Output' )  ##labels
         
         for i in range (8):
             p = P[i]   ##input operators decleared
             q = Q[i]
             r = R[i]
             
             ans = eval(data)   ##Evalutes whatever expressions that has been given in data
             
             if ans == False:   ##Changing the True or False into integers
                 ans = int(False)
             else:
                 ans = int (True)
             lst2.append(ans)
             print(P[i],' ',Q[i],' ',R[i],'  ', ans)
         print("List 2: ",lst2)

    #expression calculation
    def expression3():
         del lst3[:]    ##Deleting the list

         data = exp3.get()  ##Taking data to evaluate
         print(data)
         print('P',' ','Q',' ','R','  ','Output' )
         
         for i in range (8):
             p = P[i]   ##input operators decleared
             q = Q[i]
             r = R[i]
             
             ans = eval(data)   ##Evalutes whatever expressions that has been given in data
             
             if ans == False:   ##Changing the True or False into integers
                 ans = int(False)
             else:
                 ans = int (True)
             lst3.append(ans)
             print(P[i],' ',Q[i],' ',R[i],'  ', ans)

         print("List 3: ",lst3)

    #Function to get the conclusion and calculate it
    def conclusion():                             
        del conclude[:]

        data=conclu.get()    ##Getting data from Entry

        print('P',' ','Q',' ','R','  ','Output' )
        for i in range (8):
            p = P[i]
            q = Q[i]
            r = R[i]
            ans=eval(data)   ##Evalutes whatever expressions that has been given in data
            
            if ans == False:    ##Changing the True or False into integers
                 ans = int(False)
            else:
                 ans = int (True)
            conclude.append(ans)

            print(P[i],' ',Q[i],' ',R[i],'  ', ans)
        print("Conclusion: ",conclude)

    ##Checking if the condition is valid or not
    def check1():
        isinvalid = 0
        for a,b in zip(lst1,conclude):      ##Unlocking the lists  
            if a == 1 and b == 1:
                print("Valid")
                                
                lab_valid.pack()
                isinvalid = 0
                break
            else:
                print("a is %d, b is %d" % (a,b))
                isinvalid = 1
                
        if isinvalid == 1:
            print("Invalid")
            lab_invalid.pack()
            

    ##Checking if the condition is valid or not
    def check2():
        isinvalid = 0
        for a,b,c in zip(lst1,lst2,conclude):       ##Unlocking the lists     
            if a == 1 and b == 1:
                print("Valid")                
                lab_valid.pack()
                isinvalid = 0
                break
            else:
                print("a is %d, b is %d" % (a,b))
                isinvalid = 1       
        if isinvalid == 1:
            print("Invalid")
            lab_invalid.pack()
            
    ##Checking if the condition is valid or not
    def check3():
        isinvalid = 0
        for a,b,c,d in zip(lst1,lst2,lst3,conclude):    ##Unlocking the lists    
            if a == 1 and b == 1:
                print("Valid")               
                lab_valid.pack()
                isinvalid = 0
                break
            else:
                print("a is %d, b is %d" % (a,b))
                isinvalid = 1
                
        if isinvalid == 1:
            print("Invalid")
            lab_invalid.pack()



    def final():         ##FUNCTION THAT COMMANDS ALL FUNCTIONS WITH 1 BUTTON
        #isinvalid = 0
        lab_valid.pack_forget()
        lab_invalid.pack_forget()
        if len(lst1) and len(lst2) and len(lst3) != 0:  ##If all three permises are given
            expression1()
            expression2()
            expression3()
            conclusion()
            check3()

        elif len(lst1) and len(lst2) != 0:  ##If all two permises are given
            expression1()
            expression2()
            conclusion()
            check2()

        else:       ##If only one permises are given
            expression1()
            conclusion()            
            check1()
            
    Instructions_Label = Label(root, text = "INSTRUCTIONS",bg="tan")   ##label
    Instructions_Label.pack()    ##Making label visible
    Instructions = Label(root,fg = 'DeepPink4', text = "You can only use p q and r operators\nFor Operations Type: 'or' 'and' 'not'\nfor xor: Use ^\nFor p ↔ q:(p → q) and (q → p) \nFor p → q: Use (not(p) or q)\n ")   ##label
    Instructions.pack()    ##Making label visible
    exp1 = StringVar()
    entry1 = Entry(root,textvariable = exp1)    ##Generating Screen
    
    exp2 = StringVar()
    entry2 = Entry(root,textvariable = exp2)    ##SETTING DATA IN ENTRY
    
    exp3 = StringVar()
    entry3 = Entry(root,textvariable = exp3)   ##SETTING DATA IN ENTRY
    
    conclu = StringVar()
    entry4 = Entry(root,textvariable = conclu)    ##SETTING DATA IN ENTRY


    lab1.pack()     ##Generating Label
    entry1.pack()   ##Generating Screen
    mybutton = Button(root,padx=40, pady=10,bg="powder blue", text = "CALCULATE PREMISE 1", command=expression1) ##Setting Button
    #mybutton.pack()  ##Generating Button

    Free_label = Label(root, text = "")   ##label for expression 1
    Free_label.pack()

    lab2.pack()  ##Generating Label
    entry2.pack()   ##Generating Screen
    mybutton = Button(root,padx=40, pady=10,bg="powder blue", text = "CALCULATE PREMISE 2", command=expression2) ##Setting Button
    #mybutton.pack()##Generating Button

    Free_label = Label(root, text = "")   ##label for expression 1
    Free_label.pack()

    lab3.pack()  ##Generating Label
    entry3.pack()   ##Generating Screen
    mybutton = Button(root,padx=40, pady=10,bg="powder blue", text = "CALCULATE PPREMISE 3",compound=LEFT, command=expression3) ##Setting Button
    #mybutton.pack()##Generating Button

    Free_label = Label(root, text = "")   ##label for expression 1
    Free_label.pack()


    lab4.pack()  ##Generating Label
    entry4.pack() ##Generating Screen




    Free_label = Label(root, text = "")   ##label for expression 1
    Free_label.pack()
    mybutton2 = Button(root,padx=45, pady=10,bg="powder blue", text = "Result", command=final) ##Setting Button
    mybutton2.pack()##Generating Button       

    Free_label = Label(root, text = "")   ##label for expression 1
    Free_label.pack()
    mybutton2 = Button(root,padx=32, pady=10,bg="powder blue", text = "Main Menu", command=MainMenu) ##Setting Button
    mybutton2.pack()##Generating Button
    root.mainloop()


#Two Operator function starts
def twofunctions():
    master.destroy()
    root=Tk()
    root.geometry("280x500")
    root.wm_title("CALCULATOR FOR 2 OPERATORS") ##cHANGING THE tiltle caption

    lab1 = Label(root, text = "Expression 1")   ##label for expression 1
    lab2 = Label(root, text = "Expression 2")   ##label for expression 2
    lab3 = Label(root, text = "Expression 3")   ##label for expression 3
    lab4 = Label(root, text = "Conclusion")     ##label for Conclusion
    lab_valid = Label(root, text = "VALID")
    lab_invalid = Label(root, text = "INVALID")

    def MainMenu():
        root.destroy()
        main()

    ##Lists to show possible combination of 3-input Truth tables
    P = [0,0,1,1]
    Q = [0,1,0,1]

    lst1=[]
    lst2=[]
    lst3=[]
    conclude=[]

    def expression1():   ##Function that gets 1 expression and evaluates it
         del lst1[:]
         
         data = exp1.get()
         print(data)
         print('P',' ','Q',' ','  ','Output' )
         
         for i in range (4):
             p = P[i]
             q = Q[i]
             
             ans = eval(data)                    
                
             if ans == False:
                 ans = int(False)
             else:
                 ans = int (True)
             lst1.append(ans) 
             print(P[i],' ',Q[i],' ','  ', ans)
         print("List 1: ",lst1)


    def expression2():
         del lst2[:]
         data = exp2.get()
         print(data)
         print('P',' ','Q',' ','  ','Output' )
         
         for i in range (4):
             p = P[i]
             q = Q[i]
             
             ans = eval(data)
             
             if ans == False:
                 ans = int(False)
             else:
                 ans = int (True)
             lst2.append(ans)
             print(P[i],' ',Q[i],' ','  ', ans)

         print("List 2: ",lst2)


    def expression3():
         del lst3[:]
         data = exp3.get()
         print(data)
         print('P',' ','Q',' ','  ','Output' )
         
         for i in range (4):
             p = P[i]
             q = Q[i]
             
             ans = eval(data)
             
             if ans == False:
                 ans = int(False)
             else:
                 ans = int (True)
             lst3.append(ans)
             print(P[i],' ',Q[i],' ','  ', ans)
         print("List 3: ",lst3)


    def conclusion():                             
        del conclude[:]
        data=conclu.get()     ##Getting data from Entry

        print('P',' ','Q',' ','  ','Output' )
        for i in range (4):
            p = P[i]
            q = Q[i]
            ans=eval(data)
            
            if ans == False:
                 ans = int(False)
            else:
                 ans = int (True)
            conclude.append(ans)

            print(P[i],' ',Q[i],' ','  ', ans)
        print("Conclusion: ",conclude)

    def check1():
        isinvalid = 0
        for a,b in zip(lst1,conclude):        
            if a == 1 and b == 1:
                print("Valid")               
                lab_valid.pack()
                isinvalid = 0
                break
            else:
                print("a is %d, b is %d" % (a,b))
                isinvalid = 1
                
        if isinvalid == 1:
            print("Invalid")
            lab_invalid.pack()
            


    def check2():
        isinvalid = 0
        for a,b,c in zip(lst1,lst2,conclude):        
            if a == 1 and b == 1:
                print("Valid")               
                lab_valid.pack()
                isinvalid = 0
                break
            else:
                print("a is %d, b is %d" % (a,b))
                isinvalid = 1
                
        if isinvalid == 1:
            print("Invalid")
            lab_invalid.pack()

    def check3():
        isinvalid = 0
        for a,b,c,d in zip(lst1,lst2,lst3,conclude):        
            if a == 1 and b == 1:
                print("Valid")               
                lab_valid.pack()
                isinvalid = 0
                break
            else:
                print("a is %d, b is %d" % (a,b))
                isinvalid = 1
                
        if isinvalid == 1:
            print("Invalid")
            lab_invalid.pack()



    def final():         ##FUNCTION THAT COMMANDS ALL FUNCTIONS WITH 1 BUTTON
        lab_valid.pack_forget()
        lab_invalid.pack_forget()
        if len(lst1) and len(lst2) and len(lst3) != 0:
            expression1()
            expression2()
            expression3()
            conclusion()
            check3()

        elif len(lst1) and len(lst2) != 0:
            expression1()
            expression2()
            conclusion()
            check2()

        else:
            expression1()
            conclusion()            
            check1()

    Instructions_Label = Label(root, text = "INSTRUCTIONS",bg="tan")   ##label
    Instructions_Label.pack()    ##Making label visible
    Instructions = Label(root,fg = 'DeepPink4', text = "You can only use p q and r operators\nFor Operations Type: 'or' 'and' 'not'\nfor xor: Use ^\nFor p ↔ q:(p → q) and (q → p) \nFor p → q: Use (not(p) or q)\n ")   ##label
    Instructions.pack()    ##Making label visible
    exp1 = StringVar()
    entry1 = Entry(root,textvariable = exp1)    ##Generating Screen
    
    exp2 = StringVar()
    entry2 = Entry(root,textvariable = exp2)    ##SETTIGN DATA IN ENTRY
    
    exp3 = StringVar()
    entry3 = Entry(root,textvariable = exp3)   ##SETTIGN DATA IN ENTRY
    
    conclu = StringVar()
    entry4 = Entry(root,textvariable = conclu)    ##ETTIGN DATA IN ENTRY


    lab1.pack()     ##Generating Label
    entry1.pack()   ##Generating Screen

    Free_label = Label(root, text = "")   ##label
    Free_label.pack()

    lab2.pack()  ##Generating Label
    entry2.pack()   ##Generating Screen

    Free_label = Label(root, text = "")   ##label
    Free_label.pack()

    lab3.pack()  ##Generating Label
    entry3.pack()   ##Generating Screen

    Free_label = Label(root, text = "")   ##label
    Free_label.pack()


    lab4.pack()  ##Generating Label
    entry4.pack() ##Generating Screen

    Free_label = Label(root, text = "")   ##label
    Free_label.pack()

    mybutton2 = Button(root,padx=40, pady=10,bg="powder blue", text = "Result", command=final) ##1 button to check all premises
    mybutton2.pack()##Generating Button

    Free_label = Label(root, text = "")   ##label
    Free_label.pack()
    mybutton2 = Button(root,padx=26, pady=10,bg="powder blue", text = "Main Menu", command=MainMenu) ##Setting Button
    mybutton2.pack()##Generating Button
    root.mainloop()


##Main window that shows the main window of program and it leads to seperate windows
def main():
    def Exit():    ##Function to Exit the window
        master.destroy()
    global master
    master=Tk()
    master.geometry("350x400")
    master.wm_title("OPERATORS SELECTION") ##cHANGING THE tiltle caption
    select = Label(master, text = "\nVALIDITY CHECKER\nAND TRUTH TABLE CALCULATOR",
                   font = "Helvetica 12 bold italic",fg='brown')
    select.pack()

    Free_label = Label(master, text = "")   ##label
    Free_label.pack()
    mybutton2 = Button(master,padx=45, pady=10,bg="powder blue", text = "Two Operators",command=twofunctions)
    #mybutton2.grid(row = 8, column  = 0)
    mybutton2.pack()

    Free_label = Label(master, text = "")   ##label 
    Free_label.pack()
    mybutton2 = Button(master,padx=40, pady=10,bg="powder blue", text = "Three Oprerators", command=ThreeFunctions)
    #mybutton2.grid(row = 8, column  = 0)
    mybutton2.pack()

    Free_label = Label(master, text = "")   ##label 
    Free_label.pack()
    mybutton2 = Button(master,padx=53, pady=10,bg="powder blue", text = "Truth Table", command=Try)
    #mybutton2.grid(row = 8, column  = 0)
    mybutton2.pack()

    Free_label = Label(master, text = "")   ##label
    Free_label.pack()
    mybutton2 = Button(master,padx=75, pady=10,bg="powder blue", text = "Exit", command=Exit)
    #mybutton2.grid(row = 8, column  = 0)
    mybutton2.pack()

    master.mainloop()

main()
