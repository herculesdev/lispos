(in-package :user)

(defstruct Solicitante
    usuario
    isDevedor
    documento_pessoa
    )
(setq solicitante_list (list ))
  

(defun ler_solicitante()
    (cls)
    (format t "Digite o identificador da pessoa: ")
    (setq pessoa_id (read))
    (setq pessoa (nth pessoa_id pessoa_list))
        (when (not pessoa)
        (write-line "Registro não encontrado")
        (return-from ler_solicitante 0)
    )

    (format t "Usuário: ")
    (setq usuario-lido (read))

    (format t "Ele está devendo?(0-SIM / 1-NÃO) ")
    (setq opcao-lido(read))
    (if (= opcao-lido 0)
      (setq devedor-lido 1)
      (setq devedor-lido 0)
    )

    (setq documento (Pessoa-documento_pessoa pessoa))

    (setq s (make-Solicitante :usuario (write-to-string usuario-lido) :isDevedor (write-to-string devedor-lido) :documento_pessoa documento))
    (return-from ler_solicitante s)
)

(defun cadastrar_solicitante ()
    (cls)
    (write-line "======[Sistema OS | CAD. SOLICITANTE]======")    
    (setq s (ler_solicitante))
    (push s solicitante_list)
    (write-line "")
    (write-line "")    
)

(defun listar_solicitantes ()
    (cls)
    (write-line "======[SISTEMA OS | LIST. SOLICITANTES]======")
    (write-line (concatenate 'string "Qtd. SOLICITANTES: " (write-to-string (list-length solicitante_list))))
    (write-line "")
    (setq contador 0)

    (dolist (item solicitante_list)
        (setq pessoa nil)
        (dolist (p pessoa_list)
            (if (string= (Solicitante-documento_pessoa item) (Pessoa-documento p))
                (setq pessoa p)
            )
        )
        (format t (concatenate 'string (write-to-string contador) " - "))
        (format t (Pessoa-nome pessoa))
        (format t ", ")
        (format t (Pessoa-documento pessoa))
        (format t ", USUARIO: ")
        (format t (write-to-string (Solicitante-usuario item)))
        (format t ", DEVEDOR: ")
        (if (/= (Solicitante-isDevedor item) 0)
          (format t "sim")
          (format t "não")
        )
        (write-line "")
        (setq contador (+ contador 1))
    )

    (write-line "")
)

(defun apagar_solicitante()
    (cls)
    (format t "Digite o identificador do registro: ")
    (setq solicitante_id (read))
    (setq solicitante (nth solicitante_id solicitante_list))

    (write solicitante)
    
    (when (not solicitante)
        (write-line "Registro não encontrado")
        (return-from apagar_solicitante 0)
    )


    (setq pessoa nil)
    (dolist (pes pessoa_list)
        (if (string= (Solicitante-documento_pessoa solicitante) (Pessoa-documento pes))
            (setq pessoa pes)
        )
    )

    (cls)
    (write-line "======[SISTEMA OS | APAG. SOLICITANTE]======")
    (format t (concatenate 'string "Posição: " (write-to-string solicitante_id)))
    
    (write-line "")
    (format t "Nome: ")
    (format t (Pessoa-nome pessoa))

    (write-line "")
    (format t "Documento: ")
    (format t (Pessoa-documento pessoa))

    (write-line "")
    (format t "Usuário: ")
    (format t (Solicitante-usuario solicitante))

    (write-line "")
    (format t "Devedor: ")
    (if (/= (Solicitante-isDevedor solicitante) 0)
        (format t "sim")
        (format t "não")
    )

    
    (write-line "")
    (format t "Apagar (0 = SIM | 1 = NÃO): ")
    (setq editar (read))

    (when (/= editar 0)
        (write-line "Saindo...")
        (write-line "")
        (return-from apagar_solicitante 0)
    )

    (setq solicitante_list (delete solicitante solicitante_list))

    (write-line "Solicitante apagado dos registros")
)