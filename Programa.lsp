(defstruct Pessoa
    nome
    documento)

(setq pessoas-list (list ))
(push (make-Pessoa :nome "Hercules Moreira" :documento "02101848686") pessoas-list)
(push (make-Pessoa :nome "Christopher José" :documento "01802346868") pessoas-list)

(defun cls () (ext:run-shell-command "clear") )
(cls)
(defun cadastrar-pessoa ()
    (cls)
    (write-line "======[Sistema OS | Cad. Pessoa]======")

    (format t "Nome: ")
    (setf nome-lido (read))
    (format t "Documento: ")
    (setf documento-lido (read))
    (setq p (make-Pessoa :nome (write-to-string nome-lido) :documento (write-to-string documento-lido)))

    (push p pessoas-list)
    (write-line "")
    (write-line "")
    
)

(defun listar-pessoas ()
    (cls)
    (write-line "======[SISTEMA OS | LIST. PESSOA]======")
    (write-line (concatenate 'string "Qtd. Pessoas: " (write-to-string (list-length pessoas-list))))
    (write-line "")
    (setq contador 0)

    (dolist (item pessoas-list)
        (format t (concatenate 'string (write-to-string contador) " - "))
        (format t (Pessoa-nome item))
        (format t ", ")
        (format t (Pessoa-documento item))
        (write-line "")
        (setq contador (+ contador 1))
    )

    (write-line "")
)

(setq opcao -1)
(loop
    (write-line "======[SISTEMA OS | MENU]======")
    (write-line "1. Cadastrar Pessoa")
    (write-line "2. Listar Pessoas")

    (write-line "3. Cadastrar Serviço")
    (write-line "4. Cadastrar OS")

    (write-line "")
    (write-line "Outros: 100. Limpar tela | 200. Sair")
    (write-line "")

    (format t "Digite a opção: ")
    (setq opcao (read))
    (write-line "")

    (if (= opcao 100)
        (cls)
    )

    (if (= opcao 200)
        (quit)
    )

    (if (= opcao 1)
        (cadastrar-pessoa)
    )

    (if (= opcao 2)
        (listar-pessoas)
    )

)