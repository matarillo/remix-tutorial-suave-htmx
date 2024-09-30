module Data

open System

type ContactMutation =
    { id: string option
      first: string option
      last: string option
      avatar: string option
      twitter: string option
      notes: string option
      favorite: bool option }

    static let empty =
        { id = None
          first = None
          last = None
          avatar = None
          twitter = None
          notes = None
          favorite = None }

    static member Empty = empty

    static member FromFormData(form: (string * string option) list) =
        let folder s (k, v) =
            match s, k with
            | Some c, "first" -> Some { c with first = v }
            | Some c, "last" -> Some { c with last = v }
            | Some c, "avatar" -> Some { c with avatar = v }
            | Some c, "twitter" -> Some { c with twitter = v }
            | Some c, "notes" -> Some { c with notes = v }
            | Some c, "favorite" ->
                match v with
                | Some v ->
                    let (r, fav) = Boolean.TryParse v
                    if r then Some { c with favorite = Some fav } else None
                | None -> Some { c with favorite = None }
            | _ -> None

        List.fold folder (Some empty) form

type ContactRecord =
    { id: string
      first: string
      last: string
      avatar: string
      twitter: string
      notes: string
      favorite: bool
      createdAt: DateTimeOffset }

let createWith (values: ContactMutation) =
    let newId () =
        Random().GetItems("0123456789abcdefghijklmnopqrstuvwxyz".AsSpan(), 7) |> String

    { ContactRecord.id = Option.defaultWith newId values.id
      first = Option.defaultValue "" values.first
      last = Option.defaultValue "" values.last
      avatar = Option.defaultValue "" values.avatar
      twitter = Option.defaultValue "" values.twitter
      notes = Option.defaultValue "" values.notes
      favorite = Option.defaultValue false values.favorite
      createdAt = DateTimeOffset.UtcNow }

let updateWith (values: ContactMutation) (origin: ContactRecord) =
    { ContactRecord.id = origin.id
      first = Option.defaultValue origin.first values.first
      last = Option.defaultValue origin.last values.last
      avatar = Option.defaultValue origin.avatar values.avatar
      twitter = Option.defaultValue origin.twitter values.twitter
      notes = Option.defaultValue origin.notes values.notes
      favorite = Option.defaultValue origin.favorite values.favorite
      createdAt = origin.createdAt }

module private FakeContacts =
    open System.Collections.Generic

    let records = Dictionary<string, ContactRecord>()
    let desc (dto: System.DateTimeOffset) = -dto.Ticks

    let getAll () =
        async {
            let rs =
                records.Values |> Seq.sortBy (fun x -> x.last, desc (x.createdAt)) |> Seq.toList

            return rs
        }

    let get id =
        async {
            let (found, value) = records.TryGetValue id
            return if found then Some value else None
        }

    let create (values: ContactMutation) =
        async {
            let contact = createWith values
            records.Add(contact.id, contact)
            return contact
        }

    let set (id: string) (values: ContactMutation) =
        async {
            let! c = get id

            let contact =
                match c with
                | Some c -> c
                | None -> failwith $"No contact found for {id}"

            let newContact = contact |> updateWith values
            records[id] <- newContact
            return newContact
        }

    let destroy (id: string) = async { records.Remove id |> ignore }

let getContacts = FakeContacts.getAll

let queryContacts (query: string) =
    async {
        let! contacts = FakeContacts.getAll ()
        let loweredQuery = query.ToLowerInvariant()

        let predicate target =
            target.first.ToLowerInvariant().Contains(loweredQuery)
            || target.last.ToLowerInvariant().Contains(loweredQuery)

        return contacts |> List.filter predicate
    }

let createEmptyContact () =
    FakeContacts.create ContactMutation.Empty

let getContact id = //FakeContacts.get id
    async {
        let! contact = FakeContacts.get id

        match contact with
        | Some c -> return Ok c
        | None -> return Error $"No contact found for {id}"
    }

let updateContact id updates =
    async {
        let! contact = FakeContacts.get id

        match contact with
        | Some _ ->
            let! c = FakeContacts.set id updates
            return Ok c
        | None -> return Error $"No contact found for {id}"
    }

let deleteContact = FakeContacts.destroy

let init () =
    let emptyMutation = ContactMutation.Empty

    let generateKey (values: ContactMutation) =
        let first = Option.defaultValue "" values.first
        let last = Option.defaultValue "" values.last
        $"{String.toLowerInvariant first}-{String.toLowerInvariant last}"

    [ { emptyMutation with
          avatar = Some "https://sessionize.com/image/124e-400o400o2-wHVdAuNaxi8KJrgtN3ZKci.jpg"
          first = Some "Shruti"
          last = Some "Kapoor"
          twitter = Some "@shrutikapoor08" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/1940-400o400o2-Enh9dnYmrLYhJSTTPSw3MH.jpg"
          first = Some "Glenn"
          last = Some "Reyes"
          twitter = Some "@glnnrys"
          favorite = Some true }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/9273-400o400o2-3tyrUE3HjsCHJLU5aUJCja.jpg"
          first = Some "Ryan"
          last = Some "Florence" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/d14d-400o400o2-pyB229HyFPCnUcZhHf3kWS.png"
          first = Some "Oscar"
          last = Some "Newman"
          twitter = Some "@__oscarnewman" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/fd45-400o400o2-fw91uCdGU9hFP334dnyVCr.jpg"
          first = Some "Michael"
          last = Some "Jackson" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/b07e-400o400o2-KgNRF3S9sD5ZR4UsG7hG4g.jpg"
          first = Some "Christopher"
          last = Some "Chedeau"
          twitter = Some "@Vjeux" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/262f-400o400o2-UBPQueK3fayaCmsyUc1Ljf.jpg"
          first = Some "Cameron"
          last = Some "Matheson"
          twitter = Some "@cmatheson" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/820b-400o400o2-Ja1KDrBAu5NzYTPLSC3GW8.jpg"
          first = Some "Brooks"
          last = Some "Lybrand"
          twitter = Some "@BrooksLybrand" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/df38-400o400o2-JwbChVUj6V7DwZMc9vJEHc.jpg"
          first = Some "Alex"
          last = Some "Anderson"
          twitter = Some "@ralex1993" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/5578-400o400o2-BMT43t5kd2U1XstaNnM6Ax.jpg"
          first = Some "Kent C."
          last = Some "Dodds"
          twitter = Some "@kentcdodds" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/c9d5-400o400o2-Sri5qnQmscaJXVB8m3VBgf.jpg"
          first = Some "Nevi"
          last = Some "Shah"
          twitter = Some "@nevikashah" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/2694-400o400o2-MYYTsnszbLKTzyqJV17w2q.png"
          first = Some "Andrew"
          last = Some "Petersen" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/907a-400o400o2-9TM2CCmvrw6ttmJiTw4Lz8.jpg"
          first = Some "Scott"
          last = Some "Smerchek"
          twitter = Some "@smerchek" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/08be-400o400o2-WtYGFFR1ZUJHL9tKyVBNPV.jpg"
          first = Some "Giovanni"
          last = Some "Benussi"
          twitter = Some "@giovannibenussi" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/f814-400o400o2-n2ua5nM9qwZA2hiGdr1T7N.jpg"
          first = Some "Igor"
          last = Some "Minar"
          twitter = Some "@IgorMinar" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/fb82-400o400o2-LbvwhTVMrYLDdN3z4iEFMp.jpeg"
          first = Some "Brandon"
          last = Some "Kish" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/fcda-400o400o2-XiYRtKK5Dvng5AeyC8PiUA.png"
          first = Some "Arisa"
          last = Some "Fukuzaki"
          twitter = Some "@arisa_dev" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/c8c3-400o400o2-PR5UsgApAVEADZRixV4H8e.jpeg"
          first = Some "Alexandra"
          last = Some "Spalato"
          twitter = Some "@alexadark" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/7594-400o400o2-hWtdCjbdFdLgE2vEXBJtyo.jpg"
          first = Some "Cat"
          last = Some "Johnson" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/5636-400o400o2-TWgi8vELMFoB3hB9uPw62d.jpg"
          first = Some "Ashley"
          last = Some "Narcisse"
          twitter = Some "@_darkfadr" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/6aeb-400o400o2-Q5tAiuzKGgzSje9ZsK3Yu5.JPG"
          first = Some "Edmund"
          last = Some "Hung"
          twitter = Some "@_edmundhung" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/30f1-400o400o2-wJBdJ6sFayjKmJycYKoHSe.jpg"
          first = Some "Clifford"
          last = Some "Fajardo"
          twitter = Some "@cliffordfajard0" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/6faa-400o400o2-amseBRDkdg7wSK5tjsFDiG.jpg"
          first = Some "Erick"
          last = Some "Tamayo"
          twitter = Some "@ericktamayo" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/feba-400o400o2-R4GE7eqegJNFf3cQ567obs.jpg"
          first = Some "Paul"
          last = Some "Bratslavsky"
          twitter = Some "@codingthirty" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/c315-400o400o2-spjM5A6VVfVNnQsuwvX3DY.jpg"
          first = Some "Pedro"
          last = Some "Cattori"
          twitter = Some "@pcattori" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/eec1-400o400o2-HkvWKLFqecmFxLwqR9KMRw.jpg"
          first = Some "Andre"
          last = Some "Landgraf"
          twitter = Some "@AndreLandgraf94" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/c73a-400o400o2-4MTaTq6ftC15hqwtqUJmTC.jpg"
          first = Some "Monica"
          last = Some "Powell"
          twitter = Some "@indigitalcolor" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/cef7-400o400o2-KBZUydbjfkfGACQmjbHEvX.jpeg"
          first = Some "Brian"
          last = Some "Lee"
          twitter = Some "@brian_dlee" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/f83b-400o400o2-Pyw3chmeHMxGsNoj3nQmWU.jpg"
          first = Some "Sean"
          last = Some "McQuaid"
          twitter = Some "@SeanMcQuaidCode" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/a9fc-400o400o2-JHBnWZRoxp7QX74Hdac7AZ.jpg"
          first = Some "Shane"
          last = Some "Walker"
          twitter = Some "@swalker326" }
      { emptyMutation with
          avatar = Some "https://sessionize.com/image/6644-400o400o2-aHnGHb5Pdu3D32MbfrnQbj.jpg"
          first = Some "Jon"
          last = Some "Jensen"
          twitter = Some "@jenseng" } ]
    |> List.map (fun x -> { x with id = Some(generateKey x) })
    |> List.map FakeContacts.create
    |> Async.Sequential
    |> Async.Ignore
    |> Async.Start
