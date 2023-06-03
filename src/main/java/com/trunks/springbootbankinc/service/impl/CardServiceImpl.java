package com.trunks.springbootbankinc.service.impl;

import java.util.Calendar;
import java.util.Date;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.springframework.stereotype.Service;

import com.trunks.springbootbankinc.domain.Card;
import com.trunks.springbootbankinc.domain.Customer;
import com.trunks.springbootbankinc.domain.DocumentType;
import com.trunks.springbootbankinc.domain.TransactionHistory;
import com.trunks.springbootbankinc.domain.TransactionType;
import com.trunks.springbootbankinc.dto.TransactionInfoDTO;
import com.trunks.springbootbankinc.exception.BadRequestAlertException;
import com.trunks.springbootbankinc.repository.CardRepository;
import com.trunks.springbootbankinc.repository.CustomerRepository;
import com.trunks.springbootbankinc.service.CardService;

import lombok.extern.log4j.Log4j2;

@Log4j2
@Service
public class CardServiceImpl implements CardService{
	
	private final CardRepository cardRepository;
	
	private final CustomerRepository customerRepository;
	
	public CardServiceImpl(CardRepository cardRepository, CustomerRepository customerRepository) {
		this.cardRepository = cardRepository;
		this.customerRepository = customerRepository;
	}

	@Override
	public Card save(Card card) {
		
		return cardRepository.save(card);
	}

	@Override
	public Optional<Card> findByCardNumber(String number) {
		
		return cardRepository.findByCardNumber(number);
	}
	
	@Override
	public Optional<Card> findCardByTransactionNumber(String transactionId) {

		return cardRepository.findCardByTransactionNumber(transactionId);
	}

	@Override
	public void validateCardNumberParams(String productId, String document, String documentType) {
		
        //validate productId
        String cadPattern = "^\\d{6}$";
        Pattern pattern = Pattern.compile(cadPattern);
        Matcher matcher = pattern.matcher(productId);
        
        if(!matcher.matches()) {
        	throw new BadRequestAlertException("The productId must has a numeric value of six numbers", "Card", "productId format");
        }
        
        //validate docType
        cadPattern = "^(CC|CE)$";
        pattern = Pattern.compile(cadPattern);
        matcher = pattern.matcher(documentType.toUpperCase());
        
        if(!matcher.matches()) {
        	throw new BadRequestAlertException("The doctype must be cc or ce", "Card", "doctype format");
        }
        
        //validate document
        cadPattern = "^\\d+$";
        pattern = Pattern.compile(cadPattern);
        matcher = pattern.matcher(document);
        
        if(!matcher.matches()) {
        	throw new BadRequestAlertException("The document must be a number", "Card", "document format");
        }
	}
	
	@Override
	public Customer validateDbCustomer(String documentType, String document) {
		
        String docType = documentType.toUpperCase();
        DocumentType finalDocType = null;
        if(docType.equals(DocumentType.CEDULACIUDADANIA.getCode())) {
        	finalDocType = DocumentType.CEDULACIUDADANIA;
        } else if(docType.equals(DocumentType.CEDULAEXTRANJERIA.getCode())) {
        	finalDocType = DocumentType.CEDULAEXTRANJERIA;
        }
        
        //verify that the user exists in dB
        Optional<Customer> optionalCustomer = customerRepository.findByDocTypeDoc(document, finalDocType);
        optionalCustomer.ifPresentOrElse(
        		v -> log.debug("Customer:" + v.getId() + ", consulted"),
        		() ->  { throw new BadRequestAlertException("The customer didn´t exist", "Customer", "docTypeDocNull"); }
        );
        
        return optionalCustomer.orElse(new Customer());
	}
	
	@Override
	public Card buidCard(String idCard, String productid, Customer customer) {
		
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(new Date());
        calendar.add(Calendar.YEAR, 3);
        
        TransactionHistory transactionHistory = TransactionHistory.builder()
        	    .date(new Date())
        		.transactionType(TransactionType.NUMBERGENERATION)
        		.accountBalance(Float.valueOf("0.0"))
        		.enroll(false)
        		.block(false)
        		.build();
        
        Card card = Card.builder()
                .number(idCard)
                .idProducto(productid)
                .transactionType(TransactionType.NUMBERGENERATION)
                .accountBalance(Float.valueOf("0.0"))
                .customer(customer)
                .creationDate(new Date())
                .dueDate(calendar.getTime())
                .enroll(false)
                .block(false)
        		.build();
        
        card.addTrx(transactionHistory);
        transactionHistory.setCard(card);
		
        return card;
	}

	@Override
	public void validateProductIdInCustomerCard(String productid, Customer customer) {

		//verify that the productId doesn't in the list of user cards
        Optional<String> optionalIdProducto = customer.getCards().stream()
        	.map(Card::getIdProducto)
        	.filter(v -> v!=null && v.equals(productid))
        	.findAny();
        
        optionalIdProducto.ifPresent( v -> { throw new BadRequestAlertException("The productId already exists in a customer card", "Customer", "productId duplicated"); } );
	}

	@Override
	public void validateCardInDb(String idCard) {
		
        Optional<Card> optionalCard = findByCardNumber(idCard);
        optionalCard.ifPresent( v -> { throw new BadRequestAlertException("The card exist for the user", "Card", "findByCardNumber"); } );
	}

	@Override
	public TransactionInfoDTO buildTransactionDTOResponse(Card card) {
		
        Optional<TransactionHistory> optionaltrxHistory = card.getTransactionHistories().stream()
            	.filter(v -> v.getTransactionNumber()!=null)
            	.filter(v -> v.getTransactionNumber().equals(card.getTransactionNumber()))
            	.findAny();
        TransactionHistory trxHistory = optionaltrxHistory.orElseThrow(() -> new BadRequestAlertException("The transaction id don't exists", "TransactionHistory", "trxIdNull"));
            
        return TransactionInfoDTO.builder()
        		.date(trxHistory.getDate())
        		.cardNumber(card.getNumber())
        		.transactionNumber(trxHistory.getTransactionNumber())
        		.accountBalance(card.getAccountBalance())
        		.transactionType(trxHistory.getTransactionType())
        		.build();
	}

}
