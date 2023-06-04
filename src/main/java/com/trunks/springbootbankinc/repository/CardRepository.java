package com.trunks.springbootbankinc.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.trunks.springbootbankinc.domain.Card;

@Repository
public interface CardRepository extends JpaRepository<Card, Long>{

	@Query("select card from Card card where card.number =:number")
	Optional<Card> findByCardNumber(@Param("number") String number);

	@Query("select trxh.card from TransactionHistory trxh left join trxh.card where trxh.transactionNumber =:transactionId")
	Optional<Card> findCardByTransactionNumber(@Param("transactionId") String transactionId);
}
