/* History
Once upon a time a farmer went to a market and purchased a wolf, a goat, and a cabbage. 
On his way home, the farmer came to the bank of a river and rented a boat. But crossing 
the river by boat, the farmer could carry only himself and a single one of his 
purchases: the wolf, the goat, or the cabbage.
If left unattended together, the wolf would eat the goat, or the goat would eat the 
cabbage.
The farmer's challenge was to carry himself and his purchases to the far bank of the 
river, leaving each purchase intact. How did he do it?
--------------------
initialState: River1 [0, 0, 0, 0, 0], River2 [1, 1, 1, 1, 1]
target State: River2 [0, 0, 0, 0, 0], River2 [1, 1, 1, 1, 1]
--------------------
Where:
setn[0]: boat
setn[1]: farmer
setn[2]: cabage
setn[3]: sheep
setn[4]: wolf
--------------------
Invalid states:
[x,0,x,1,1]
[x,0,1,1,x]
where x can take any value
--------------------
Transition
[n, n, x, x, x]
where n is mandatory and just one
x or none can be traded
Disclaimer: All values must be swaped
in the same position
*/
const initialState = { 
    set1: [false, false, false, false, false], 
    set2: [true, true, true, true, true] 
}
const targetState = { 
    set1: [false, false, false, false, false], 
    set2: [true, true, true, true, true] 
}
let processedStates = []
/*

*/
class Node {
    constructor(state, root = null) {
        this.curState = state
        this.root = root
        this.childs = []
        processedStates.push(this.getGlobalState(this.curState))
        this.genPaths()
    }
    genPaths() {
        if(this.curState.set1[1]) this.applyOp(this.curState.set1, this.curState.set2, true)
        else this.applyOp(this.curState.set2, this.curState.set1, false)
    }
    applyOp(from, to, flow) {
        let fromCopy = [...from]
        let toCopy = [...to]
        fromCopy[0] = false
        fromCopy[1] = false
        toCopy[0] = true
        toCopy[1] = true
        if(this.isValid(fromCopy)) {
            if(flow) this.childs.push({set1:fromCopy, set2:toCopy, operator: 3})
            else this.childs.push({set1:toCopy, set2:fromCopy, operator: 3})
        }
        for(let i=0; i<3; i++) {
            if(from[i+2]) {
                fromCopy = [...from]
                toCopy = [...to]
                fromCopy[i+2] = !fromCopy[i+2]
                toCopy[i+2] = !toCopy[i+2]
                fromCopy[0] = false
                fromCopy[1] = false
                toCopy[0] = true
                toCopy[1] = true
                if(this.isValid(fromCopy)) {
                    if(flow) this.childs.push({set1:fromCopy, set2:toCopy, operator: i})
                    else this.childs.push({set1:toCopy, set2:fromCopy, operator: i})  
                }
            }
        }
        this.childs = this.childs.filter(state => 
            !processedStates.includes(this.getGlobalState(state))
        )
    }
    isValid(state) {
        if(!state[0] && state[3] && state[4]) return false
        else if(!state[0] && state[2] && state[3]) return false
        else return true
    }
    getGlobalState(state) {
        let res = ''
        state.set1.forEach(val => {
            res += val ? '1': '0'
        })
        state.set2.forEach(val => {
            res += val ? '1': '0'
        })
        return parseInt(res, 2);
    }
    getHumanState() {
        if(this.curState.operator === 0) console.log('Moves boatt, farmer and cabbage')
        else if(this.curState.operator === 1) console.log('Moves boat, farmer goat')
        else if(this.curState.operator === 2) console.log('Moves boat, farmer and wolf')
        else if(this.curState.operator === 3) console.log('Moves boat and farmer')
        else console.log('There is no preceding operator')
        console.log('River bank 1:')
        this.curState.set1[0] ? console.log('The boat') : 0
        this.curState.set1[1] ? console.log('The farmer') : 0
        this.curState.set1[2] ? console.log('The cabbage') : 0
        this.curState.set1[3] ? console.log('The goat') : 0
        this.curState.set1[4] ? console.log('The wolf') : 0
        console.log('---------------')
        console.log('River bank 2:')
        this.curState.set2[0] ? console.log('The boat') : 0
        this.curState.set2[1] ? console.log('The farmer') : 0
        this.curState.set2[2] ? console.log('The cabbage') : 0
        this.curState.set2[3] ? console.log('The goat') : 0
        this.curState.set2[4] ? console.log('The wolf') : 0
        console.log('\n')
    }
}
const proccessNode = (node) => {
    if(node.getGlobalState(node.curState) === 992) {
        backTracking(node)
    } else {
        if(processedStates.includes(992)) return
        node.childs.forEach(state => {
            let child = new Node(state, node)
            proccessNode(child)
        })
    }
}
const backTracking = (node) => {
    node.getHumanState()
    if(node.root !== null) {
        backTracking(node.root)
    } else return
}
let init = new Node(initialState)
proccessNode(init)