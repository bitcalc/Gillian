'use strict';

/*************************
 *************************
 *******           *******
 *******   needs   *******
 *******           *******
 *************************
 *************************/

/**
 * @id needs
 */
function needs (condition, errorMessage) {
    if (!condition) {
        throw new Error(errorMessage)
    }
}

/********************************
 ********************************
 *******                  *******
 *******   readElements   *******
 *******                  *******
 ********************************
 ********************************/

/**
    @id readElements

    @pred nounfold innerLoopInvariantElement(+definition, +remainingElsList, +view, +outerLoopReadPos, +fCount, fList, eLength) :
      (definition == "Complete") * CElement(view, outerLoopReadPos, fCount, fList, eLength),
      (definition == "Incomplete") * (remainingElsList == {{ }}) * IElement(view, outerLoopReadPos, fCount, fList, eLength),
      (definition == "Incomplete") * (! (remainingElsList == {{ }})) * CElement(view, outerLoopReadPos, fCount, fList, eLength);

    @pre
      (elementCount == #eCount) * (fieldsPerElement == #fCount) * (buffer == #buffer) * (readPos == #readPos) *
      Uint8Array (#buffer, #ab, #viewOffset, #viewSize) *
      ArrayBuffer(#ab, #data) *
      (#view == l-sub(#data, #viewOffset, #viewSize)) *
      Elements(#definition, #view, #readPos, #eCount, #fCount, #eList, #esLength) *

      scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
      JSInternals ()

    @post
      (#definition == "Complete") *
      Uint8Array (#buffer, #ab, #viewOffset, #viewSize) *
      ArrayBuffer(#ab, #data) *
      Elements(#definition, #view, #readPos, #eCount, #fCount, #eList, #esLength) *
      scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
      JSInternals () *

      JSObject(ret) *
        DataProp(ret, "elements", #elements) *
            ArrayOfArraysOfUInt8Arrays(#elements, #eList) *
        DataProp(ret, "readPos", #ret_readPos) *
            (#ret_readPos == #readPos + #esLength);

      (#definition == "Incomplete") *
      Uint8Array (#buffer, #ab, #viewOffset, #viewSize) *
      ArrayBuffer(#ab, #data) *
      Elements(#definition, #view, #readPos, #eCount, #fCount, #eList, #esLength) *

      scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
      JSInternals () *

      (ret == false)
*/
function readElements(elementCount, fieldsPerElement, buffer, readPos) {
  /* @tactic apply lemma ElementsFacts(#view, #readPos, #eCount, #fCount, #eList, #esLength) */
  var dataView = new DataView(
      buffer.buffer,
      buffer.byteOffset,
      buffer.byteLength
  );

  /* Well-formedness: readPos must be within the byte length of the buffer given. */
  needs(readPos >= 0 && dataView.byteLength >= readPos, 'readPos out of bounds.')

  /* Well-formedness: elementCount must not be negative. */
  needs(elementCount >= 0, 'elementCount must be positive.')

  var elements = [];

  /*
    @invariant
      scope(elements : #doneEls) * scope(readPos : #outerLoopReadPos) * scope(elementCount : #eLeft) *
      CElements(#view, #readPos, #eCount - #eLeft, #fCount, #doneElsList, #doneElsLength) *
      Elements(#definition, #view, #outerLoopReadPos, #eLeft, #fCount, #remainingElsList, #remainingElsLength) *
      (#eList == l+ (#doneElsList, #remainingElsList)) *
      (#esLength == #doneElsLength + #remainingElsLength) *
      (#readPos + #doneElsLength == #outerLoopReadPos) *
      ArrayOfArraysOfUInt8Arrays(#doneEls, #doneElsList)
      [bind : #doneEls, #outerLoopReadPos, #eLeft, #remainingElsList, #remainingElsLength, #doneElsList, #doneElsLength]
  */
  while (elementCount--) {
    /* @tactic
        if (#definition = "Complete") then {
          apply lemma CElementsFacts(#view, #outerLoopReadPos, #eLeft, #fCount, #remainingElsList, #remainingElsLength);
          unfold CElements(#view, #outerLoopReadPos, #eLeft, #fCount, #remainingElsList, #remainingElsLength);
          assert CElement(#view, #outerLoopReadPos, #fCount, #fList, #eLength) [bind: #fList, #eLength]
        } else {
          unfold IElements(#view, #outerLoopReadPos, #eLeft, #fCount, #remainingElsList, #remainingElsLength);
          if (#remainingElsList = {{ }}) then {
            assert IElement(#view, #outerLoopReadPos, #fCount, #fList, #remainingElsLength) [bind: #fList]
          } else {
            assert CElement(#view, #outerLoopReadPos, #fCount, #fList, #eLength) [bind: #fList, #eLength]
          }
        }
     */
    var element = []
    var fieldCount = fieldsPerElement

    /* @invariant
        scope(element : #doneEl) * scope(readPos : #innerLoopReadPos) * scope(fieldCount : #fLeft) *
        innerLoopInvariantElement(#definition, #remainingElsList, #view, #innerLoopReadPos, #fLeft, #remainingElList, #remainingElLength) *
        CElement(#view, #outerLoopReadPos, #fCount - #fLeft, #doneElList, #doneElLength) *
        (#fList == l+ (#doneElList, #remainingElList)) *
        (#eLength == #doneElLength + #remainingElLength) *
        (#outerLoopReadPos + #doneElLength == #innerLoopReadPos) *
        ArrayOfUInt8Arrays(#doneEl, #doneElList, #fCount - #fLeft)
        [bind: #doneEl, #innerLoopReadPos, #fLeft, #remainingElList, #remainingElLength, #doneElList, #doneElLength] */
    while (fieldCount--) {
      /* @tactic
          if (#definition = "Complete") then {
            unfold CElement(#view, #innerLoopReadPos, #fLeft, #remainingElList, #remainingElLength)
          };
          if ((#definition = "Incomplete") and (#remainingElsList = {{ }})) then {
            unfold IElement(#view, #innerLoopReadPos, #fLeft, #remainingElList, #remainingElLength)
          }
      */
      /* Check for early return (Postcondition): Enough data must exist to read the Uint16 length value. */
      if (readPos + 2 > dataView.byteLength)
        /*
          @tactic
            apply lemma PrependCElementI(#view, #outerLoopReadPos, (#fCount - #fLeft), #doneElList, #doneElLength, #fLeft, #remainingElList, #remainingElLength);
            assert IElement(#view, #outerLoopReadPos, #fCount, #fList, #remainingElsLength);
            assert Elements(#definition, #view, #outerLoopReadPos, #eLeft, #fCount, #remainingElsList, #remainingElsLength);
            apply lemma PrependCElementsE(#definition, #view, #readPos, (#eCount - #eLeft), #fCount, #doneElsList, #doneElsLength, #eLeft, #remainingElsList, #remainingElsLength)
         */
        return false
      var length = dataView.getUint16(readPos, false) // big endian
      readPos += 2
      /* Check for early return (Postcondition): Enough data must exist length of the value. */
      if (readPos + length > dataView.byteLength)
        /*
          @tactic
            apply lemma PrependCElementI(#view, #outerLoopReadPos, (#fCount - #fLeft), #doneElList, #doneElLength, #fLeft, #remainingElList, #remainingElLength);
            assert IElement(#view, #outerLoopReadPos, #fCount, #fList, #remainingElsLength);
            assert Elements(#definition, #view, #outerLoopReadPos, #eLeft, #fCount, #remainingElsList, #remainingElsLength);
            apply lemma PrependCElementsE(#definition, #view, #readPos, (#eCount - #eLeft), #fCount, #doneElsList, #doneElsLength, #eLeft, #remainingElsList, #remainingElsLength)
         */
        return false
      var fieldBinary = buffer.slice(readPos, readPos + length)
      readPos += length
      /* @tactic
         assert (#remainingElList == l+ ({{ #fld }}, #rfld)) [bind: #fld, #rfld];
         if (#definition = "Complete") then {
           apply lemma AppendFieldCC(#view, #outerLoopReadPos, #fCount - #fLeft, #doneElList, #doneElLength, #fLeft, #fld, #rfld, #remainingElLength)
         } else {
           apply lemma AppendFieldCI(#view, #outerLoopReadPos, #fCount - #fLeft, #doneElList, #doneElLength, #fLeft, #fld, #rfld, #remainingElLength)
         }
      */
      element.push(fieldBinary)
    }
    /* @tactic
        if (#definition = "Complete") then {
          unfold CElement(#view, #innerLoopReadPos, #fLeft, #remainingElList, #remainingElLength);
          apply lemma CElementsAppend(#view, #readPos, (#eCount - #eLeft), #fCount, #doneElsList, #doneElsLength, #doneElList, #doneElLength)
        } else {
          unfold IElement(#view, #innerLoopReadPos, #fLeft, #remainingElList, #remainingElLength)
        }
    */
    elements.push(element);
  }

  /*
    @tactic
    if (#definition = "Complete") then {
      unfold CElements(#view, #outerLoopReadPos, #eLeft, #fCount, #remainingElsList, #remainingElsLength)
    } else {
      unfold IElements(#view, #outerLoopReadPos, #eLeft, #fCount, #remainingElsList, #remainingElsLength)
    }
  */
  return { elements, readPos }
}

/************************************
 ************************************
 *******                      *******
 *******   EncryptedDataKey   *******
 *******                      *******
 ************************************
 ************************************/

/**
    @id EncryptedDataKey

    @onlyspec EncryptedDataKey (edk)
    [[
        (edk == #edk) *
        JSObject(#edk) *
        DataProp(#edk, "providerId", #pId) * types(#pId : Str) *
        DataProp(#edk, "providerInfo", #pInfo) * types(#pInfo : Str) *
        DataProp(#edk, "encryptedDataKey", #aEDK) *
            Uint8Array (#aEDK, #abEDK, 0, #viewSizeEDK) *
            ArrayBuffer(#abEDK, #encryptedDataKey) *
            (#viewSizeEDK == l-len #encryptedDataKey) *
        DataProp(#edk, "rawInfo", #aRInfo) *
            Uint8Array (#aRInfo, #abRInfo, 0, #viewSizeRInfo) *
            ArrayBuffer(#abRInfo, #rawInfo) *
            (#viewSizeRInfo == l-len #rawInfo) *
        JSObjWithProto (this, $l_edk_proto)
    ]]
    [[
        JSObject(#edk) *
        DataProp(#edk, "providerId", #pId) *
        DataProp(#edk, "providerInfo", #pInfo) *
        DataProp(#edk, "encryptedDataKey", #aEDK) *
            Uint8Array (#aEDK, #abEDK, 0, #viewSizeEDK) *
            ArrayBuffer(#abEDK, #encryptedDataKey) *
        DataProp(#edk, "rawInfo", #aRInfo) *
            Uint8Array (#aRInfo, #abRInfo, 0, #viewSizeRInfo) *
            ArrayBuffer(#abRInfo, #rawInfo) *

        EncryptedDataKey(this, #pId, #pInfo, #encryptedDataKey, #rawInfo) *
        (ret == this)
    ]]
    normal
*/
var EncryptedDataKey = function (edk) { };

/**************************
 **************************
 *******            *******
 *******   toUtf8   *******
 *******            *******
 **************************
 **************************/

 /**
    @id toUtf8

    @onlyspec toUtf8 (buffer)
        [[
            (buffer == #buffer) *
            Uint8Array (#buffer, #ab, 0, #length) *
            ArrayBuffer(#ab, #element)
        ]]
        [[
            Uint8Array (#buffer, #ab, 0, #length) *
            ArrayBuffer(#ab, #element) *
            toUtf8(#element, ret)
        ]]
        normal
*/
var toUtf8 = function (buffer) { };

/*******************************************
 *******************************************
 *******                             *******
 *******   decodeEncryptionContext   *******
 *******                             *******
 *******************************************
 *******************************************/

/**
    @id decodeEncryptionContext

    @pre (this == undefined) * (encodedEncryptionContext == #eEC) *
         Uint8Array(#eEC, #aBuffer, #byteOffset, #byteLength) * ArrayBuffer(#aBuffer, #data) *
         (#EC == l-sub(#data, #byteOffset, #byteLength)) *
         (#definition == "Complete") *
         RawEncryptionContext(#definition, #EC, #ECKs, #errorMessage) *

         scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
         scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
         JSInternals()

    @post Uint8Array(#eEC, #aBuffer, #byteOffset, #byteLength) * ArrayBuffer(#aBuffer, #data) *
          RawEncryptionContext(#definition, #EC, #ECKs, #errorMessage) *

          DecodedEncryptionContext(ret) *

          scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
          scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
          JSInternals ()

    @pre (this == undefined) * (encodedEncryptionContext == #eEC) *
         Uint8Array(#eEC, #aBuffer, #byteOffset, #byteLength) * ArrayBuffer(#aBuffer, #data) *
         (#EC == l-sub(#data, #byteOffset, #byteLength)) *
         (#definition == "Broken") *
         RawEncryptionContext(#definition, #EC, #ECKs, #errorMessage) *

         scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
         scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
         JSInternals()

    @posterr
          Uint8Array(#eEC, #aBuffer, #byteOffset, #byteLength) * ArrayBuffer(#aBuffer, #data) *
          RawEncryptionContext(#definition, #EC, #ECKs, #errorMessage) *

          ErrorObjectWithMessage(ret, #errorMessage) *

          scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
          scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
          JSInternals ()
*/
function decodeEncryptionContext(encodedEncryptionContext) {
  /* @tactic
      if (#definition = "Complete") then {
        unfold CRawEncryptionContext(#EC, #ECKs)
      } else {
        unfold BRawEncryptionContext(#errorMessage, #EC)
      }
   */

  var encryptionContext = Object.create(null)
  /* Check for early return (Postcondition): The case of 0 length is defined as an empty object. */
  if (!encodedEncryptionContext.byteLength) {
    return encryptionContext
  }
  /* Uint8Array is a view on top of the underlying ArrayBuffer.
    * This means that raw underlying memory stored in the ArrayBuffer
    * may be larger than the Uint8Array.  This is especially true of
    * the Node.js Buffer object.  The offset and length *must* be
    * passed to the DataView otherwise I will get unexpected results.
    */
  var dataView = new DataView(
    encodedEncryptionContext.buffer,
    encodedEncryptionContext.byteOffset,
    encodedEncryptionContext.byteLength
  )
  var pairsCount = dataView.getUint16(0, false) // big endian
  var elementInfo = readElements(pairsCount, 2, encodedEncryptionContext, 2)
  /* Postcondition: Since the encryption context has a length, it must have pairs.
    * Unlike the encrypted data key section, the encryption context has a length
    * element.  This means I should always pass the entire section.
    */
  if (!elementInfo) throw new Error('decodeEncryptionContext: Underflow, not enough data.')
  var { elements, readPos } = elementInfo

  /* Postcondition: The byte length of the encodedEncryptionContext must match the readPos. */
  needs(
    encodedEncryptionContext.byteLength === readPos,
    'decodeEncryptionContext: Overflow, too much data.'
  )

  /*
    @tactic
        assert ((#EC == l+ ({{ #b0, #b1 }}, #rest)) * Elements("Complete", #EC, 2, ((256 * #b0) + #b1), 2, #ECKs, l-len #rest)) [bind: #b0, #b1, #rest];
        unfold Elements("Complete", #EC, 2, ((256 * #b0) + #b1), 2, #ECKs, l-len #rest);
        apply lemma CElementsFacts(#EC, 2, ((256 * #b0) + #b1), 2, #ECKs, l-len #rest);
        assert (
            scope(pairsCount: #pairsCount) * (#pairsCount == l-len #ECKs) *
            scope(elements: #elements) * ArrayOfArraysOfUInt8Arrays(#elements, #ECKs) *
            scope(encryptionContext: #dECObj) * JSObjWithProto(#dECObj, null) * empty_fields(#dECObj : -{ }-)
        ) [bind: #pairsCount, #elements, #dECObj]

    @invariant
        scope(count: #count) * (#count <=# #pairsCount) *
        ArrayOfArraysOfUInt8ArraysContents(#elements, #done, 0, #count) *
        ArrayOfArraysOfUInt8ArraysContents(#elements, #left, #count, #pairsCount - #count) *
        (#ECKs == l+ (#done, #left)) [bind: #count, #done, #left] */
  for (var count = 0; count < pairsCount; count++) {
    /*
        @tactic
        apply lemma ArrayOfArraysOfUInt8ArraysContentsFacts(#elements, #done, 0, #count);
        apply lemma ArrayOfArraysOfUInt8ArraysContentsFacts(#elements, #left, #count, #pairsCount - #count);
        unfold ArrayOfArraysOfUInt8ArraysContents(#elements, #left, #count, #pairsCount - #count)
    */
   var pre_stuff = 0;

  //   var [key, value] = elements[count].map(toUtf8)
  //   /* Postcondition: The number of keys in the encryptionContext must match the pairsCount.
  //     * If the same Key value is serialized...
  //     */
  //   needs(
  //     encryptionContext[key] === undefined,
  //     'Duplicate encryption context key value.'
  //   )
  //   encryptionContext[key] = value

    /*
        @tactic
        apply lemma ArrayOfArraysOfUInt8ArraysContentsAppend(#elements, #done, 0, #count);
        apply lemma IntegerLtPlusOneLe(#count, #pairsCount)
    */
    var post_stuff = 0;
  }
  /* @tactic assert (scope(count : (l-len #ECKs))) */
  // Object.freeze(encryptionContext)
  return encryptionContext
}

/************************************************
 ************************************************
 *******                                  *******
 *******   deserializeEncryptedDataKeys   *******
 *******                                  *******
 ************************************************
 ************************************************/

/**
    @id deserializeEncryptedDataKeys

    @pre (this == undefined) * (buffer == #buffer) * (startPos == #startPos) *
         Uint8Array(#buffer, #aBuffer, #byteOffset, #byteLength) * ArrayBuffer(#aBuffer, #data) *
         (#view == l-sub(#data, #byteOffset, #byteLength)) *
         ((#definition == "Complete") \/ (#definition == "Incomplete")) *
         RawEncryptedDataKeys(#definition, #view, #startPos, #EDKs, #EDKsLength, #errorMessage) *

         scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
         scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
         JSInternals()

    @post Uint8Array(#buffer, #aBuffer, #byteOffset, #byteLength) * ArrayBuffer(#aBuffer, #data) *
          (#view == l-sub(#data, #byteOffset, #byteLength)) *
          RawEncryptedDataKeys(#definition, #view, #startPos, #EDKs, #EDKsLength, #errorMessage) *

          (#definition == "Complete") *
          JSObject(ret) *
            DataProp(ret, "encryptedDataKeys", #dEDKs) *
                DeserialisedEncryptedDataKeys(#dEDKs, #EDKs) *
            DataProp(ret, "readPos", #startPos + #EDKsLength) *

          scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
          scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
          JSInternals ();

          Uint8Array(#buffer, #aBuffer, #byteOffset, #byteLength) * ArrayBuffer(#aBuffer, #data) *
          (#view == l-sub(#data, #byteOffset, #byteLength)) *
          RawEncryptedDataKeys(#definition, #view, #startPos, #EDKs, #EDKsLength, #errorMessage) *

          (#definition == "Incomplete") * (ret == false) *

          scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
          scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
          JSInternals ()

    @pre (this == undefined) * (buffer == #buffer) * (startPos == #startPos) *
         Uint8Array(#buffer, #aBuffer, #byteOffset, #byteLength) * ArrayBuffer(#aBuffer, #data) *
         (#view == l-sub(#data, #byteOffset, #byteLength)) *
         (#definition == "Broken") *
         RawEncryptedDataKeys(#definition, #view, #startPos, #EDKs, #EDKsLength, #errorMessage) *

         scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
         JSInternals()

    @posterr
          Uint8Array(#buffer, #aBuffer, #byteOffset, #byteLength) * ArrayBuffer(#aBuffer, #data) *
          (#view == l-sub(#data, #byteOffset, #byteLength)) *
          RawEncryptedDataKeys(#definition, #view, #startPos, #EDKs, #EDKsLength, #errorMessage) *

          ErrorObjectWithMessage(ret, #errorMessage) *

          scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
          JSInternals ()
*/
function deserializeEncryptedDataKeys(buffer, startPos) {
  /* @tactic
      unfold RawEncryptedDataKeys(#definition, #view, #startPos, #EDKs, #EDKsLength, #errorMessage);
      if (#definition = "Complete") then {
        unfold CRawEncryptedDataKeys(#view, #startPos, #EDKs, #EDKsLength)
      } else {
          if (#definition = "Incomplete") then {
            unfold IRawEncryptedDataKeys(#view, #startPos)
          } else {
            unfold BRawEncryptedDataKeys(#errorMessage, #view, #startPos)
          }
      } */

  /* Precondition: startPos must be within the byte length of the buffer given. */
  needs(
    buffer.byteLength >= startPos && startPos >= 0,
    'deserializeMessageHeader: startPos out of bounds.'
  )

  /* Check for early return (Postcondition): Need to have at least Uint16 (2) bytes of data. */
  if (startPos + 2 > buffer.byteLength) return false

  /* Uint8Array is a view on top of the underlying ArrayBuffer.
   * This means that raw underlying memory stored in the ArrayBuffer
   * may be larger than the Uint8Array.  This is especially true of
   * the Node.js Buffer object.  The offset and length *must* be
   * passed to the DataView otherwise I will get unexpected results.
   */
  var dataView = new DataView(
    buffer.buffer,
    buffer.byteOffset,
    buffer.byteLength
  )
  var encryptedDataKeysCount = dataView.getUint16(startPos, false) // big endian

  /* Precondition: There must be at least 1 EncryptedDataKey element. */
  needs(encryptedDataKeysCount, 'Malformed Header: No EncryptedDataKey found.')

  var elementInfo = readElements(
    encryptedDataKeysCount,
    3,
    buffer,
    startPos + 2
  )
  /* Check for early return (Postcondition): readElement will return false if there is not enough data.
   * I can only continue if I have at least the entire EDK section.
   */
  if (!elementInfo) return false
  var { elements, readPos } = elementInfo

  var encryptedDataKeys = elements.map(
    /**
        @id aux_deserializeEncryptedDataKey

        @pre (element == #element) *
             ArrayOfUInt8Arrays (#element, {{ #rawId, #rawInfo, #encryptedDataKey }}, 3) *
             scope(toUtf8: #toUtf8) * JSFunctionObject(#toUtf8, "toUtf8", #t_sc, #t_len, #t_proto) *
             scope(EncryptedDataKey: #EncryptedDataKey) * JSFunctionObject(#EncryptedDataKey, "EncryptedDataKey", #e_sc, #e_len, $l_edk_proto) *
             GlobalObject()

        @post ArrayOfUInt8Arrays (#element, {{ #rawId, #rawInfo, #encryptedDataKey }}, 3) *
              scope(toUtf8: #toUtf8) * JSFunctionObject(#toUtf8, "toUtf8", #t_sc, #t_len, #t_proto) *
              scope(EncryptedDataKey: #EncryptedDataKey) * JSFunctionObject(#EncryptedDataKey, "EncryptedDataKey", #e_sc, #e_len, $l_edk_proto) *
              GlobalObject() *

              toUtf8(#rawId, #pId) * toUtf8(#rawInfo, #pInfo) *
              EncryptedDataKey(ret, #pId, #pInfo, #encryptedDataKey, #rawInfo)
    */
    element => {
      /* FIXME: Implement the array deconstructor */
      var [rawId, rawInfo, encryptedDataKey] = element
      var providerId = toUtf8(rawId)
      var providerInfo = toUtf8(rawInfo)
      return new EncryptedDataKey({
        providerInfo,
        providerId,
        encryptedDataKey,
        rawInfo,
      })
    }
  )

  Object.freeze(encryptedDataKeys)
  return { encryptedDataKeys, readPos }
}

/********************************************
 ********************************************
 *******                              *******
 *******   AlgorithmSuiteIdentifier   *******
 *******                              *******
 ********************************************
 ********************************************/

var AlgorithmSuiteIdentifier;

/****************************
 ****************************
 *******              *******
 *******   SdkSuite   *******
 *******              *******
 ****************************
 ****************************/

/*
    @onlyspec SdkSuite (suiteId)
        [[
            JSObject(this) * ((this, "ivLength") -> none) * ((this, "tagLength") -> none) *
            CAlgorithmSuite(suiteId, #stringId, #ivLength, #tagLength)
        ]]
        [[
            AlgorithmSuiteObject(this, #ivLength, #tagLength) *
            (ret == this)
        ]]
        normal
 */
var SdkSuite = function (suiteId) { };

/********************************************
 ********************************************
 *******                              *******
 *******   deserializeMessageHeader   *******
 *******                              *******
 ********************************************
 ********************************************/

/**
    @id deserializeMessageHeader

    @pre (messageBuffer == #messageBuffer) *
         Uint8Array(#messageBuffer, #buffer, #byteOffset, #byteLength) * ArrayBuffer(#buffer, #data) *
         (#view == l-sub(#data, #byteOffset, #byteLength)) *
         (#byteOffset + #byteLength <=# l-len #data) *
         ((#definition == "Complete") \/ (#definition == "Incomplete")) *
         Header(#definition,
                #view, #part_one, #version, #type, #suiteId, #messageId, #rECLength,
                       #part_two, #ECKs,
                       #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag,
                #errorMessage) *

         scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
         scope(AlgorithmSuiteIdentifier : #ASIObject) * AlgorithmSuiteIdentifierObject(#ASIObject) *
         scope(decodeEncryptionContext : #DECObject) * JSFunctionObject(#DECObject, "decodeEncryptionContext", #dc_sc, #dc_len, #dc_proto) *
         scope(deserializeEncryptedDataKeys : #DEDKObject) * JSFunctionObject(#DEDKObject, "deserializeEncryptedDataKeys", #ds_sc, #ds_len, #ds_proto) *
         scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
         scope(SdkSuite : #SdkObject) * JSFunctionObject(#SdkObject, "SdkSuite", #s_sc, #s_len, $lobj_proto) *
         JSInternals ()

    @post Uint8Array(#messageBuffer, #buffer, #byteOffset, #byteLength) * ArrayBuffer(#buffer, #data) *
          Header(#definition,
                 #view, #part_one, #version, #type, #suiteId, #messageId, #rECLength,
                        #part_two, #ECKs,
                        #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag,
                 #errorMessage) *

          (#definition == "Complete") *
          HeaderInfo(ret, #version, #type, #suiteId, #messageId, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #rawHeaderData, #headerIv, #headerAuthTag) *
          (#rawHeaderData == l-sub(#view, 0, #headerLength)) *

          scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
          scope(AlgorithmSuiteIdentifier : #ASIObject) * AlgorithmSuiteIdentifierObject(#ASIObject) *
          scope(decodeEncryptionContext : #DECObject) * JSFunctionObject(#DECObject, "decodeEncryptionContext", #dc_sc, #dc_len, #dc_proto) *
          scope(deserializeEncryptedDataKeys : #DEDKObject) * JSFunctionObject(#DEDKObject, "deserializeEncryptedDataKeys", #ds_sc, #ds_len, #ds_proto) *
          scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
          scope(SdkSuite : #SdkObject) * JSFunctionObject(#SdkObject, "SdkSuite", #s_sc, #s_len, $lobj_proto) *
          JSInternals();

          Uint8Array(#messageBuffer, #buffer, #byteOffset, #byteLength) * ArrayBuffer(#buffer, #data) *
          Header(#definition,
                 #view, #part_one, #version, #type, #suiteId, #messageId, #rECLength,
                        #part_two, #ECKs,
                        #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag,
                 #errorMessage) *

          (definition == "Incomplete") * (ret == false) *

          scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
          scope(AlgorithmSuiteIdentifier : #ASIObject) * AlgorithmSuiteIdentifierObject(#ASIObject) *
          scope(decodeEncryptionContext : #DECObject) * JSFunctionObject(#DECObject, "decodeEncryptionContext", #dc_sc, #dc_len, #dc_proto) *
          scope(deserializeEncryptedDataKeys : #DEDKObject) * JSFunctionObject(#DEDKObject, "deserializeEncryptedDataKeys", #ds_sc, #ds_len, #ds_proto) *
          scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
          scope(SdkSuite : #SdkObject) * JSFunctionObject(#SdkObject, "SdkSuite", #s_sc, #s_len, $lobj_proto) *
          JSInternals()

    @pre (messageBuffer == #messageBuffer) *
         Uint8Array(#messageBuffer, #buffer, #byteOffset, #byteLength) * ArrayBuffer(#buffer, #data) *
         (#view == l-sub(#data, #byteOffset, #byteLength)) *
         (#byteOffset + #byteLength <=# l-len #data) *
         (#definition == "Broken") *
         Header(#definition,
                #view, #part_one, #version, #type, #suiteId, #messageId, #rECLength,
                       #part_two, #ECKs,
                       #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag,
                #errorMessage) *

         scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
         scope(AlgorithmSuiteIdentifier : #ASIObject) * AlgorithmSuiteIdentifierObject(#ASIObject) *
         scope(decodeEncryptionContext : #DECObject) * JSFunctionObject(#DECObject, "decodeEncryptionContext", #dc_sc, #dc_len, #dc_proto) *
         scope(deserializeEncryptedDataKeys : #DEDKObject) * JSFunctionObject(#DEDKObject, "deserializeEncryptedDataKeys", #ds_sc, #ds_len, #ds_proto) *
         scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
         scope(SdkSuite : #SdkObject) * JSFunctionObject(#SdkObject, "SdkSuite", #s_sc, #s_len, $lobj_proto) *
         JSInternals ()

    @posterr
          Uint8Array(#messageBuffer, #buffer, #byteOffset, #byteLength) * ArrayBuffer(#buffer, #data) *
          Header(#definition,
                 #view, #part_one, #version, #type, #suiteId, #messageId, #rECLength,
                        #part_two, #ECKs,
                        #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag,
                 #errorMessage) *

          ErrorObjectWithMessage(ret, #errorMessage) *

          scope(needs : #needs) * JSFunctionObject(#needs, "needs", #n_sc, #n_len, #n_proto) *
          scope(AlgorithmSuiteIdentifier : #ASIObject) * AlgorithmSuiteIdentifierObject(#ASIObject) *
          scope(decodeEncryptionContext : #DECObject) * JSFunctionObject(#DECObject, "decodeEncryptionContext", #dc_sc, #dc_len, #dc_proto) *
          scope(deserializeEncryptedDataKeys : #DEDKObject) * JSFunctionObject(#DEDKObject, "deserializeEncryptedDataKeys", #ds_sc, #ds_len, #ds_proto) *
          scope(readElements : #readElements) * JSFunctionObject(#readElements, "readElements", #rE_sc, #rE_len, #rE_proto) *
          scope(SdkSuite : #SdkObject) * JSFunctionObject(#SdkObject, "SdkSuite", #s_sc, #s_len, $lobj_proto) *
          JSInternals()
*/
function deserializeMessageHeader(messageBuffer) {
    /* Uint8Array is a view on top of the underlying ArrayBuffer.
    * This means that raw underlying memory stored in the ArrayBuffer
    * may be larger than the Uint8Array.  This is especially true of
    * the Node.js Buffer object.  The offset and length *must* be
    * passed to the DataView otherwise I will get unexpected results.
    */
    var dataView = new DataView(
      messageBuffer.buffer,
      messageBuffer.byteOffset,
      messageBuffer.byteLength
    )

    /* @tactic unfold Header(#definition, #view, #part_one, #version, #type, #suiteId, #messageId, #rECLength, #part_two, #ECKs, #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag, #errorMessage) */
    /* @tactic
        if (#definition = "Complete") then {
          unfold CHeader(#view, #part_one, #version, #type, #suiteId, #messageId, #rECLength, #part_two, #ECKs, #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag)
        } else { if (#definition = "Incomplete") then {
            unfold IHeader(#view, #part_one, #version, #type, #suiteId, #messageId, #rECLength, #part_two, #ECKs, #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag)
          } else {
            unfold BHeader(#view, #part_one, #version, #type, #suiteId, #messageId, #rECLength, #part_two, #ECKs, #part_three, #EDKs, #contentType, #headerIvLength, #frameLength, #headerLength, #headerIv, #headerAuthTag, #errorMessage)
          }
        } */

    /* Check for early return (Postcondition): Not Enough Data. Need to have at least 22 bytes of data to begin parsing.
    * The first 22 bytes of the header are fixed length.  After that
    * there are 2 variable length sections.
    */
    if (dataView.byteLength < 22) return false // not enough data

    var version = dataView.getUint8(0)
    var type = dataView.getUint8(1)
    /* Precondition: version and type must be the required values. */
    needs(
      version === 1 && type === 128,
      version === 65 && type === 89
        ? 'Malformed Header: This blob may be base64 encoded.'
        : 'Malformed Header: Unsupported version and/or type.'
    )

    var suiteId = dataView.getUint16(2, false)  // big endian
    /* Precondition: suiteId must match supported algorithm suite */
    needs(AlgorithmSuiteIdentifier[suiteId], 'Malformed Header: Unsupported algorithm suite.')
    var messageId = messageBuffer.slice(4, 20)
    var contextLength = dataView.getUint16(20, false) // big endian

    /* Check for early return (Postcondition): Not Enough Data. Need to have all of the context in bytes before we can parse the next section.
    * This is the first variable length section.
    */
    if (22 + contextLength > dataView.byteLength) return false // not enough data

    var encryptionContext = decodeEncryptionContext(
      messageBuffer.slice(22, 22 + contextLength)
    )

    var dataKeyInfo = deserializeEncryptedDataKeys(
      messageBuffer,
      22 + contextLength
    )

    /* Check for early return (Postcondition): Not Enough Data. deserializeEncryptedDataKeys will return false if it does not have enough data.
    * This is the second variable length section.
    */
    if (!dataKeyInfo) return false // not enough data

    var { encryptedDataKeys, readPos } = dataKeyInfo

    /* I'm doing this here, after decodeEncryptionContext and deserializeEncryptedDataKeys
     * because they are the bulk of the header section.
     */
    var algorithmSuite = new SdkSuite(suiteId)
    var { ivLength, tagLength } = algorithmSuite
    var tagLengthBytes = tagLength / 8
    var headerLength = readPos + 1 + 4 + 1 + 4

    /* Check for early return (Postcondition): Not Enough Data. Need to have the remaining fixed length data to parse. */
    if (headerLength + ivLength + tagLengthBytes > dataView.byteLength)
      return false // not enough data

    var contentType = dataView.getUint8(readPos)
    var reservedBytes = dataView.getUint32(readPos + 1, false) // big endian
    /* Postcondition: reservedBytes are defined as 0,0,0,0
     * See: https://docs.aws.amazon.com/encryption-sdk/latest/developer-guide/message-format.html#header-reserved
     */
    needs(reservedBytes === 0, 'Malformed Header: Reserved bytes not equal to zero.')
    var headerIvLength = dataView.getUint8(readPos + 1 + 4)
    /* Postcondition: The headerIvLength must match the algorithm suite specification. */
    needs(headerIvLength === ivLength, 'Malformed Header: Mismatch between expected and obtained IV length.')
    var frameLength = dataView.getUint32(readPos + 1 + 4 + 1, false) // big endian
    var rawHeader = messageBuffer.slice(0, headerLength)

    var messageHeader = {
      version,
      type,
      suiteId,
      messageId,
      encryptionContext,
      encryptedDataKeys,
      contentType,
      headerIvLength,
      frameLength,
    }

    var headerIv = messageBuffer.slice(headerLength, headerLength + ivLength)
    var headerAuthTag = messageBuffer.slice(
      headerLength + ivLength,
      headerLength + ivLength + tagLengthBytes
    )

    return {
      messageHeader,
      headerLength,
      rawHeader,
      algorithmSuite,
      headerIv,
      headerAuthTag,
    }
}