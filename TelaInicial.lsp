




(push (make-Endereco :id 0 :cep "38550-156" :rua "Rua Ficticia em Paracatu") endereco_list)
(push (make-Pessoa :nome "Hercules Moreira" :documento "02101848686" :endId 0) pessoa_list)

(push (make-Endereco :id 1 :cep "38755-000" :rua "Rua Pedro Alvares Cabral") endereco_list)
(push (make-Pessoa :nome "Christopher José" :documento "01802346868" :endId 1) pessoa_list)

(push (make-Solicitante :usuario "chriscoy" :isDevedor 0 :documento_pessoa "01802346868") solicitante_list)

(defun cls () (ext:run-shell-command "clear") )
(cls)


(setq opcao -1)
(loop
    (write-line "======[SISTEMA OS | MENU]======")
    (write-line "1. Cadastrar Pessoa")
    (write-line "2. Listar Pessoas")
    (write-line "3. Editar Pessoa")
    (write-line "4. Apagar Pessoa")
    (write-line "")
    (write-line "5. Cadastrar Solicitante")
    (write-line "6. Listar Solicitantes")
    (write-line "7. Editar Solicitantes")
    (write-line "8. Apagar Solicitantes")

    (write-line "")
    (write-line "9. Cadastrar Executante")
    (write-line "10. Listar Executantes")
    (write-line "11. Editar Executante")
    (write-line "12. Apagar Executante")

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

    (if (= opcao 5)
        (cadastrar_solicitante)
    )

    (if (= opcao 6)
        (listar_solicitantes)
    )

    (if (= opcao 7)
        (editar_solicitante)
    )

    (if (= opcao 8)
        (apagar_solicitante)
    )
    
    (if (= opcao 9)
        (cadastrar_executante)
    )

    (if (= opcao 10)
        (listar_executantes)
    )

    (if (= opcao 11)
        (editar_executante)
    )

    (if (= opcao 12)
        (apagar_executante)
    )
)




; TEST AREA