import Rupture

while True :
    text = input("Rupture> ")
    result, error = Rupture.run('<stdin>', text)

    if error : print(error.AsString())
    elif result : print(result)