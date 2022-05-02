(defstruct Endereco
    id
    cep
    rua)

(defstruct Pessoa
    nome
    documento
    endId)

(setq pessoa_list (list ))
(setq endereco_list (list ))

(push (make-Endereco :id 0 :cep "38550-156" :rua "Rua Ficticia em Paracatu") endereco_list)
(push (make-Pessoa :nome "Hercules Moreira" :documento "02101848686" :endId 0) pessoa_list)

(push (make-Endereco :id 1 :cep "38755-000" :rua "Rua Pedro Alvares Cabral") endereco_list)
(push (make-Pessoa :nome "Christopher José" :documento "01802346868" :endId 1) pessoa_list)

(defun cls () (ext:run-shell-command "clear") )
(cls)

(defun ler_endereco(&optional indice)
    (format t "Rua: ")
    (setq rua-lido (read))

    (format t "Cep: ")
    (setq cep-lido (read))

    (if indice
        (setq idEnd indice)
        (setq idEnd (+ (list-length endereco_list) 1))
    )

    (setq e (make-Endereco :id idEnd :rua (write-to-string rua-lido) :cep (write-to-string cep-lido)))
    (return-from ler_endereco e)
)

(defun ler_pessoa(&optional indice)
    (format t "Nome: ")
    (setq nome-lido (read))
    (format t "Documento: ")
    (setq documento-lido (read))
    
    (if indice
        (setq idEnd indice)
        (setq idEnd (+ (list-length endereco_list) 1))
    )

    (setq p (make-Pessoa :nome (write-to-string nome-lido) :documento (write-to-string documento-lido) :endId idEnd))
    (return-from ler_pessoa p)
)

(defun cadastrar_pessoa ()
    (cls)
    (write-line "======[Sistema OS | CAD. PESSOA]======")    
    (setq p (ler_pessoa))
    (push p pessoa_list)
    (setq e (ler_endereco 42))
    (push e endereco_list)
    (write-line "")
    (write-line "")    
)

(defun listar_pessoas ()
    (cls)
    (write-line "======[SISTEMA OS | LIST. PESSOA]======")
    (write-line (concatenate 'string "Qtd. Pessoas: " (write-to-string (list-length pessoa_list))))
    (write-line "")
    (setq contador 0)

    (dolist (item pessoa_list)
        (setq e (nth contador endereco_list))
        (format t (concatenate 'string (write-to-string contador) " - "))
        (format t (Pessoa-nome item))
        (format t ", ")
        (format t (Pessoa-documento item))
        (format t ", ")
        (format t (write-to-string (Endereco-rua e)))
        (format t " CEP:")
        (format t (write-to-string (Endereco-cep e)))
        (write-line "")
        (setq contador (+ contador 1))
    )

    (write-line "")
)

(defun editar_pessoa()
    (cls)
    (format t "Digite o identificador do registro: ")
    (setq pessoa_id (read))
    (setq pessoa (nth pessoa_id pessoa_list))
    (setq endereco (nth pessoa_id endereco_list))

    (when (not pessoa)
        (write-line "Registro não encontrado")
        (return-from editar_pessoa 0)
    )

    (cls)
    (write-line "======[SISTEMA OS | EDIT. PESSOA]======")
    (format t (concatenate 'string "Posição: " (write-to-string pessoa_id)))
    
    (write-line "")
    (format t "Nome: ")
    (format t (Pessoa-nome pessoa))

    (write-line "")
    (format t "Documento: ")
    (format t (Pessoa-documento pessoa))

    (write-line "")
    (format t "Rua: ")
    (format t (Endereco-rua endereco))

    (write-line "")
    (format t "CEP: ")
    (format t (Endereco-cep endereco))

    (write-line "")
    (format t "End-ID: ")
    (format t (write-to-string (Pessoa-endId pessoa)))
    
    (write-line "")
    (format t "Editar (0 = SIM | 1 = NÃO): ")
    (setq editar (read))

    (when (/= editar 0)
        (write-line "Saindo...")
        (write-line "")
        (return-from editar_pessoa 0)
    )

    (setq pessoa_lida (ler_pessoa (Pessoa-endId pessoa)))
    (write pessoa_lida)
    (setf (nth pessoa_id pessoa_list) pessoa_lida)
    (write-line "")
)

(defun apagar_pessoa()
    (cls)
    (format t "Digite o identificador do registro: ")
    (setq pessoa_id (read))
    (setq pessoa (nth pessoa_id pessoa_list))
    (setq endereco (nth pessoa_id endereco_list))

    (when (not pessoa)
        (write-line "Registro não encontrado")
        (return-from apagar_pessoa 0)
    )

    (cls)
    (write-line "======[SISTEMA OS | APAG. PESSOA]======")
    (format t (concatenate 'string "Posição: " (write-to-string pessoa_id)))
    
    (write-line "")
    (format t "Nome: ")
    (format t (Pessoa-nome pessoa))

    (write-line "")
    (format t "Documento: ")
    (format t (Pessoa-documento pessoa))

    (write-line "")
    (format t "Rua: ")
    (format t (Endereco-rua endereco))

    (write-line "")
    (format t "CEP: ")
    (format t (Endereco-cep endereco))

    (write-line "")
    (format t "End-ID: ")
    (format t (write-to-string (Pessoa-endId pessoa)))
    
    (write-line "")
    (format t "Apagar (0 = SIM | 1 = NÃO): ")
    (setq editar (read))

    (when (/= editar 0)
        (write-line "Saindo...")
        (write-line "")
        (return-from editar_pessoa 0)
    )

    (setq pessoa_list (delete pessoa pessoa_list))
    (setq endereco_list (delete endereco endereco_list))

    (write-line "Pessoa apagada dos registros")
)

(setq opcao -1)
(loop
    (write-line "======[SISTEMA OS | MENU]======")
    (write-line "1. Cadastrar Pessoa")
    (write-line "2. Listar Pessoas")
    (write-line "3. Editar Pessoa")
    (write-line "4. Apagar Pessoa")

    (write-line "5. Cadastrar Serviço")
    (write-line "6. Cadastrar OS")

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
        (cadastrar_pessoa)
    )

    (if (= opcao 2)
        (listar_pessoas)
    )

    (if (= opcao 3)
        (editar_pessoa)
    )

    (if (= opcao 4)
        (apagar_pessoa)
    )

)




; TEST AREA