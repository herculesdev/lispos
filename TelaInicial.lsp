(push (make-Endereco :id 0 :cep "38550-156" :rua "Rua Ficticia em Paracatu") endereco_list)
(push (make-Pessoa :nome "Hercules Moreira" :documento "02101848686" :endId 0) pessoa_list)

(push (make-Endereco :id 1 :cep "38755-000" :rua "Rua Pedro Alvares Cabral") endereco_list)
(push (make-Pessoa :nome "Christopher José" :documento "01802346868" :endId 1) pessoa_list)

(push (make-Solicitante :usuario "chriscoy" :isDevedor 0 :documento_pessoa "01802346868") solicitante_list)

(defun cls () (ext:run-shell-command "clear") )
(cls)

(defun menuPessoa()
    (cls)
    (write-line "1. Cadastrar Pessoa")
    (write-line "2. Listar Pessoas")
    (write-line "3. Editar Pessoa")
    (write-line "4. Apagar Pessoa")
    (write-line "5. Voltar")
    (setq opcao2 (read))

    (if (= opcao2 1)
        (cadastrar_pessoa)
    )

    (if (= opcao2 2)
        (listar_pessoas)
    )

    (if (= opcao2 3)
        (editar_pessoa)
    )

    (if (= opcao2 4)
        (apagar_pessoa)
    )
)

(defun menuSolicitante()
    (cls)
    (write-line "1. Cadastrar Solicitante")
    (write-line "2. Listar Solicitantes")
    (write-line "3. Editar Solicitantes")
    (write-line "4. Apagar Solicitantes")        
    (write-line "5. Voltar")
    (setq opcao2 (read))

    (if (= opcao2 1)
        (cadastrar_solicitante)
    )

    (if (= opcao2 2)
        (listar_solicitantes)
    )

    (if (= opcao2 3)
        (editar_solicitante)
    )

    (if (= opcao2 4)
        (apagar_solicitante)
    )    
)

(defun menuExecutante()
    (write-line "1. Cadastrar Executante")
    (write-line "2. Listar Executantes")
    (write-line "3. Editar Executante")
    (write-line "4. Apagar Executante")
    (setq opcao2 (read))

    (if (= opcao2 1)
        (cadastrar_executante)
    )

    (if (= opcao2 2)
        (listar_executantes)
    )

    (if (= opcao2 3)
        (editar_executante)
    )

    (if (= opcao2 4)
        (apagar_executante)
    )
)

(defun menuSO()
    (write-line "1. Cadastrar S0")
    (write-line "2. Listar S0")
    (write-line "3. Editar S0")
    (write-line "4. Apagar S0")
    (setq opcao2 (read))
    
    (if (= opcao2 1)
        (cadastrar_ordemservico)
    )

    (if (= opcao2 2)
        (listar_ordemservicos)
    )

    (if (= opcao2 3)
        (editar_ordemservico)
    )

    (if (= opcao2 4)
        (apagar_ordemservico)
    )
)

(setq opcao -1)
(loop
    (write-line "======[SISTEMA OS | MENU]======")
    (write-line "1. Pessoa")
    (write-line "2. Solicitante")
    (write-line "3. Executante")
    (write-line "4. S0")

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
        (menuPessoa)
    )

    (if (= opcao 2)
        (menuSolicitante)
    )

    (if (= opcao 3)
        (menuExecutante)
    )

    (if (= opcao 4)
        (menuSO)
    )
)

; TEST AREA