![TeX-LaMbDa logo](/img/texlambda-logo.png)

texlambda es un paquete de LaTeX para darle formato a términos bien formados del cálculo lambda sin tipos.

Es necesario tener instalado Racket para crear el ejecutable `texlambda`, pero el código fuente puede ser modificado para ejecutarlo como script.

Este repositorio debe de ser copiado en `/usr/share/texmf-dist/tex/latex/` para ser utilizado y el ejecutable `texlambda` debe estar en la variable de entorno `PATH`.

Si tienes contemplado utilizar este paquete en un sistema operativo como Windows o MacOS, puedes crear el ejecutable a partir del archivo `texlambda.rkt` de la siguiente manera:

```bash
$ raco exe -o texlambda texlambda.rkt
```

O utilizando DrRacket haciendo clic en `Racket - Create Executable... - Create` con las opciones `Stand-alone` en `Type`  y `Racket` en `Base`.

---

texlambda is a LaTeX package for typesetting well formed terms in the untyped lambda calculus.

It requires Racket to create the `texlambda` executable, but the source file may be modified to 
be run as a script.

To understand how to use the package you must learn how to speak spanish and read the `test.pdf` file.
