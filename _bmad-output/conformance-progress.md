---
currentTier: CONF-4
currentStory: CONF-4.6
storiesComplete: 0
storiesTotal: 45
lastVerified: ""
---

# z/OSMF Conformance Progress

## TIER 1: Missing Endpoints
- [ ] CONF-1.1: Console — GET solicited messages endpoint
- [ ] CONF-1.2: Console — GET unsolicited message detection
- [ ] CONF-1.3: TSO — Ping session endpoint
- [ ] CONF-1.4: Dataset — Rename operation
- [ ] CONF-1.5: Dataset — Copy operation
- [ ] CONF-1.6: Dataset — Migrate operation
- [ ] CONF-1.7: Dataset — Recall operation
- [ ] CONF-1.8: Job — Submit from dataset
- [ ] CONF-1.9: USS — chmod operation
- [ ] CONF-1.10: USS — chown operation
- [ ] CONF-1.11: USS — chtag operation
- [ ] CONF-1.12: USS — Copy file/directory
- [ ] CONF-1.13: USS — Move/Rename file/directory

## TIER 2: Missing Fields
- [ ] CONF-2.1: Dataset list — Add missing item fields
- [ ] CONF-2.2: Dataset list — Remove extra totalRows field
- [ ] CONF-2.3: Dataset create — Add missing allocation fields
- [ ] CONF-2.4: Job response — Add missing exec/step fields
- [ ] CONF-2.5: Job response — Add step-data support
- [ ] CONF-2.6: Job feedback — Add missing fields
- [ ] CONF-2.7: TSO start response — Add missing fields
- [ ] CONF-2.8: TSO v1 response — Add missing fields
- [ ] CONF-2.9: USS entry — Add tag field
- [ ] CONF-2.10: Error response — Add details and stack fields
- [ ] CONF-2.11: Console request — Add missing optional fields
- [ ] CONF-2.12: Console response — Add cmd-response-uri field
- [ ] CONF-2.13: Spool file — Verify all fields present

## TIER 3: Missing Query Parameters & Headers
- [ ] CONF-3.1: Dataset list — Add volser and start query params
- [ ] CONF-3.2: Member list — Add pattern and start query params
- [ ] CONF-3.3: Job list — Add max-jobs and exec-data query params
- [ ] CONF-3.4: Dataset read — Add X-IBM-Data-Type header support
- [ ] CONF-3.5: Dataset write — Add X-IBM-Data-Type header support
- [ ] CONF-3.6: USS read — Add X-IBM-Data-Type header support
- [ ] CONF-3.7: USS write — Add X-IBM-Data-Type header support
- [ ] CONF-3.8: USS delete — Add X-IBM-Option recursive header
- [ ] CONF-3.9: USS create — Parse JSON request body
- [ ] CONF-3.10: Dataset list — Fix X-IBM-Attributes logic
- [ ] CONF-3.11: Dataset/USS — Add ETag response headers
- [ ] CONF-3.12: Dataset/USS — Add If-Match write support

## TIER 4: Behavioral Fixes
- [ ] CONF-4.1: USS write — Distinguish create vs update status code
- [ ] CONF-4.2: Dataset write — Distinguish create vs update for members
- [ ] CONF-4.3: Login response — Match IBM behavior
- [ ] CONF-4.4: Dataset list — Add X-IBM-Response-Rows header
- [ ] CONF-4.5: X-IBM-Max-Items — Return 206 Partial Content
- [x] CONF-4.6: Dataset PUT — Differentiate action vs content write
- [ ] CONF-4.7: USS PUT — Differentiate action vs content write
