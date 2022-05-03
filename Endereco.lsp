;;endereco basicamente tem um id e pessoa também tem, assim
;;que o endereco é associado a pessoa

;;tanto ler endereco quanto ler pessoa pode receber um atributo opcional,
;;esse atributo é para quando for editar, se vier algo no atributo
;;quer dizer que é pra manter o ID do endereco, se não vier nada, o valor é atribuido
;;automaticamente por meio do lenght da lista de endereco

(defstruct Endereco
    id
    cep
    rua)
(setq endereco_list (list ))

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
