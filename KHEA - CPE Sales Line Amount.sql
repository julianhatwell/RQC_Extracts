-----------------------------------------------
-- KHEA Sales Line - Amount (Course Amount)
-----------------------------------------------
SELECT sh.[sell-to contact no_] AS sellTo
	, sl.[Student No_] AS contactId
   , sl.[Document No_] AS DocNo
   , sl.[No_] AS [LineItem]
   , case when patindex('%[0-9]', sl.[NO_]) > 1 then LEFT(sl.[No_], LEN(sl.[No_]) - 1) else sl.[no_]  end AS LineType
   , sl.[Variant Code] AS Variant
   , sl.[Gen_ Prod_ Posting Group] AS PostingGroup
   , sl.[Amount]
   , sl.[Amortization Start Date] AS AmortStartDate
   , sl.[Amortization End Date] AS AmortEndDate
   
FROM [APMIS$Sales Line] sl
INNER JOIN [APMIS$SALES HEADER] SH ON sh.[No_] = sl.[Document No_] 

where  
sl.[No_] <> '' 
AND SH.[Document Type] = 1
AND sl.[Amount] <> 0
AND sh.[sell-to contact no_] <> ''
AND (
	SL.[Gen_ Prod_ Posting Group] = 'COURSE'
			OR
			(
				sh.[CANCEL] = 0 
				AND 
					(
					SL.[Gen_ Prod_ Posting Group] = 'REBATES'
					or SL.[Gen_ Prod_ Posting Group] = 'UPGREBATES'
					or SL.[Gen_ Prod_ Posting Group] = 'ENHANCE'
					)
			)
	)